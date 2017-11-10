extern crate clap;
extern crate indicatif;
extern crate blake2;
extern crate rand;
extern crate png;
extern crate nalgebra as na;
extern crate data_encoding;
extern crate rayon;

use std::fs::File;
use std::path::Path;
use std::io::BufWriter;
use std::mem;
use clap::{Arg, App};
use blake2::{Blake2b, Digest};
use rand::{Rng, SeedableRng, XorShiftRng, Closed01};
use rand::distributions::{Range, Gamma, IndependentSample};
use png::HasParameters;
use data_encoding::HEXUPPER;
use indicatif::{ProgressBar, ProgressStyle};

mod expr;

use expr::{Expr, Color};

fn main() {
    fn check_nat(x: String) -> Result<(), String> {
        x.parse().map(|_: u32| ()).map_err(|e| e.to_string())
    }
    fn check_float(x: String) -> Result<(), String> {
        x.parse().map(|_: f32| ()).map_err(|e| e.to_string())
    }
    fn check_hex(x: String) -> Result<(), String> {
        if HEXUPPER.decode_len(x.as_bytes().len()).map_err(|e| e.to_string())? != 16 {
            return Err("incorrect length; should be 32 hex digits".into());
        }
        let mut tmp = [0; 16];
        HEXUPPER.decode_mut(x.as_bytes(), &mut tmp).map(|_| ()).map_err(|e| e.error.to_string())
    }

    let args = App::new("rustart")
        .version("0.1")
        .author("Benjamin Saunders <ben.e.saunders@gmail.com>")
        .about("Art generator")
        .arg(Arg::with_name("FILE")
             .help("File to write PNG output to.")
             .required(true))
        .arg(Arg::with_name("style")
             .long("style")
             .takes_value(true)
             .help("String to seed the style pRNG with. If unsupplied, a random seed is used.")
             .value_name("string")
             .display_order(4))
        .arg(Arg::with_name("raw style")
             .long("raw-style")
             .validator(check_hex)
             .takes_value(true)
             .help("Raw 128-bit pRNG seed")
             .conflicts_with("seed")
             .display_order(5)
             .value_name("hex"))
        .arg(Arg::with_name("params")
             .long("params")
             .takes_value(true)
             .help("String to seed the parameterization pRNG with. If unsupplied, a random seed is used.")
             .value_name("string")
             .display_order(4))
        .arg(Arg::with_name("raw params")
             .long("raw-param")
             .validator(check_hex)
             .takes_value(true)
             .help("Raw 128-bit pRNG parameterization seed")
             .conflicts_with("seed")
             .display_order(5)
             .value_name("hex"))
        .arg(Arg::with_name("width")
             .help("Width of the image to generate")
             .short("w")
             .validator(check_nat)
             .default_value("1024")
             .display_order(0))
        .arg(Arg::with_name("height")
             .help("Height of the image to generate")
             .short("h")
             .validator(check_nat)
             .default_value("768")
             .display_order(1))
        .arg(Arg::with_name("zoom")
             .help("Inverse scale factor applied to the image. Larger = more view.")
             .short("z")
             .validator(check_float)
             .default_value("1.0")
             .display_order(2))
        .arg(Arg::with_name("depth")
             .help("Complexity of the generated image.")
             .short("d")
             .validator(check_nat)
             .default_value("7")
             .display_order(3))
        .get_matches();

    let width = args.value_of("width").unwrap().parse().unwrap();
    let height = args.value_of("height").unwrap().parse().unwrap();
    let zoom = args.value_of("zoom").unwrap().parse().unwrap();
    let depth = args.value_of("depth").unwrap().parse().unwrap();

    let file = File::create(Path::new(args.value_of_os("FILE").unwrap())).expect("couldn't open output file");
    let mut encoder = png::Encoder::new(BufWriter::new(file), width, height);
    encoder.set(png::ColorType::RGB).set(png::BitDepth::Eight);
    let mut encoder = encoder.write_header().expect("couldn't write png header");

    let style_seed = get_seed("style", args.value_of("raw style"), args.value_of("style"));
    let param_seed = get_seed("params", args.value_of("raw params"), args.value_of("params"));

    let mut style_rng = XorShiftRng::from_seed(style_seed);
    let mut param_rng = XorShiftRng::from_seed(param_seed);
    let mut data = vec![Color([0.0; 3]); (width*height) as usize];
    let expr = generate(&mut style_rng, &mut param_rng, depth).simplify();
    println!("program: {}", expr);
    render(expr, &mut data, width as usize, height as usize, zoom);

    let spinner = ProgressBar::new_spinner();
    spinner.set_message("encoding...");
    spinner.enable_steady_tick(100);
    let data = data.iter().map(|&Color(x)| [convert(x[0]), convert(x[1]), convert(x[2])]).collect::<Vec<[u8; 3]>>();

    encoder.write_chunk(*b"sRGB", &[0]).unwrap();
    encoder.write_image_data(flatten(&data)).expect("couldn't write image data");
    spinner.finish_and_clear();
}

fn get_seed(name: &'static str, raw: Option<&str>, text: Option<&str>) -> [u32; 4] {
    if let Some(x) = raw {
        let mut seed: [u8; 16] = unsafe { mem::uninitialized() };
        HEXUPPER.decode_mut(x.as_bytes(), &mut seed).unwrap();
        unsafe { mem::transmute::<[u8; 16], [u32; 4]>(seed) }
    } else if let Some(x) = text {
        let mut hasher = Blake2b::default();
        hasher.input(x.as_bytes());
        let x = hasher.result();
        println!("{}:\t{}", name, HEXUPPER.encode(&x[0..16]));
        unsafe { *(x.as_ptr() as *const [u32; 4]) }
    } else {
        let seed: [u8; 16] = rand::random();
        println!("{}:\t{}", name, HEXUPPER.encode(&seed));
        unsafe { mem::transmute::<[u8; 16], [u32; 4]>(seed) }
    }
}

fn flatten(xs: &[[u8; 3]]) -> &[u8] {
    unsafe { std::slice::from_raw_parts(xs.as_ptr() as *const u8, xs.len() * 3) }
}

fn convert(x: f32) -> u8 {
    let x = if x <= 0.0031308 { 12.92 * x } else { 1.055 * x.powf(1.0 / 2.4) - 0.055 }; // linear to gamma
    let x = if x <= 0.0 { 0.0 } else if x >= 1.0 { 1.0 } else { x };
    (x * 255.0) as u8
}

fn render(e: Expr, out: &mut [Color], width: usize, height: usize, zoom: f32) {
    let bar = ProgressBar::new(height as u64);
    bar.set_style(ProgressStyle::default_bar()
                  .template("{spinner} [{elapsed_precise}] {percent:>3}% {wide_bar} {eta:>3}"));

    let aspect = width as f32 / height as f32;
    let scale = na::Matrix3::new_nonuniform_scaling(&na::Vector2::new(zoom * aspect, zoom));
    let xf = na::Affine2::from_matrix_unchecked(scale);
    let e = Expr::Transform(Box::new(e), xf);

    use rayon::prelude::*;
    let job_height = height / rayon::current_num_threads();

    out.par_chunks_mut(width * job_height)
        .enumerate()
        .for_each(|(idx, out)| {
            e.render(out, width, height, idx * job_height, job_height, |progress| { bar.inc(progress as u64); });
        });

    bar.finish_and_clear();
}

macro_rules! weighted_branch {
    { $rng:ident, $weight0:expr => $body0:expr, $( $weight:expr => $body:expr,)* } => {
        let mut weights = [$weight0, $($weight,)*];
        let total = accumulate(&mut weights);
        let mut i = choose_index($rng, &weights, total);
        if i == 0 { return $body0; }
        $(
            i -= 1;
            if i == 0 { return $body; }
        )*
        unreachable!();
    }
}

fn accumulate(xs: &mut [u32]) -> u32 {
    let mut sum = 0;
    for x in xs {
        sum += *x;
        *x = sum;
    }
    sum
}

fn choose_index<R: Rng>(rng: &mut R, weights: &[u32], total: u32) -> usize {
    let sample_weight = Range::new(0, total).ind_sample(rng);
    let mut idx = 0;
    let mut modifier = weights.len();

    // short circuit when it's the first item
    if sample_weight < weights[0] {
        return 0;
    }

    // now we know that every possibility has an element to the
    // left, so we can just search for the last element that has
    // cumulative weight <= sample_weight, then the next one will
    // be "it". (Note that this greatest element will never be the
    // last element of the vector, since sample_weight is chosen
    // in [0, total_weight) and the cumulative weight of the last
    // one is exactly the total weight.)
    while modifier > 1 {
        let i = idx + modifier / 2;
        if weights[i] <= sample_weight {
            // we're small, so look to the right, but allow this
            // exact element still.
            idx = i;
            // we need the `/ 2` to round up otherwise we'll drop
            // the trailing elements when `modifier` is odd.
            modifier += 1;
        } else {
            // otherwise we're too big, so go left. (i.e. do
            // nothing)
        }
        modifier /= 2;
    }
    idx+1
}

fn generate<R: Rng, S: Rng>(style_rng: &mut R, rng: &mut S, depth: u32) -> Expr {
    use self::Expr::*;
    if depth == 0 {
        weighted_branch!{
            style_rng,
            3 => Constant(rng.gen()),
            1 => Radial,
            1 => Square,
            1 => Cos,
            1 => Abs,
        }
    } else {
        weighted_branch!{
            style_rng,
            4 => Multiply(Box::new(generate(style_rng, rng, depth - 1)), Box::new(generate(style_rng, rng, depth - 1))),
            1 => Sum(Box::new(generate(style_rng, rng, depth - 1)), Box::new(generate(style_rng, rng, depth - 1))),
            1 => { let inner = generate(style_rng, rng, depth - 1); transform(rng, inner) },
            1 => Invert(Box::new(generate(style_rng, rng, depth - 1))),
            1 => Tile(Box::new(generate(style_rng, rng, depth - 1))),
        }
    }
}

fn transform<R: Rng>(rng: &mut R, inner: Expr) -> Expr {
    use self::Expr::Transform;
    let Closed01(r): Closed01<f32> = rng.gen();
    let rot = na::Rotation2::new(r * 2.0 * ::std::f32::consts::PI);
    let Closed01(tx): Closed01<f32> = rng.gen();
    let Closed01(ty): Closed01<f32> = rng.gen();
    let trans = na::Translation2::from_vector(na::Vector2::new(tx * 2.0 - 1.0, ty * 2.0 - 1.0));
    let gamma = Gamma::new(2.0, 2.0);
    let sx = gamma.ind_sample(rng) as f32;
    let sy = gamma.ind_sample(rng) as f32;
    let scale = na::Matrix3::new_nonuniform_scaling(&na::Vector2::new(sx, sy));
    let xf = trans * rot * na::Affine2::from_matrix_unchecked(scale);
    Transform(Box::new(inner), xf)
}
