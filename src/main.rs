extern crate clap;
extern crate indicatif;
extern crate blake2;
extern crate rand;
extern crate png;
extern crate nalgebra as na;

use std::fs::File;
use std::path::Path;
use std::io::BufWriter;
use clap::{Arg, App};
use blake2::{Blake2b, Digest};
use rand::{Rng, SeedableRng, XorShiftRng, Closed01};
use rand::distributions::{Range, Gamma, IndependentSample};
use png::HasParameters;

mod expr;

use expr::{Expr, Color};

fn main() {
    fn check_nat(x: String) -> Result<(), String> {
        x.parse().map(|_: u32| ()).map_err(|e| e.to_string())
    }
    fn check_float(x: String) -> Result<(), String> {
        x.parse().map(|_: f32| ()).map_err(|e| e.to_string())
    }

    let args = App::new("rustart")
        .version("0.1")
        .author("Benjamin Saunders <ben.e.saunders@gmail.com>")
        .about("Art generator")
        .arg(Arg::with_name("FILE")
             .help("File to write PNG output to.")
             .required(true))
        .arg(Arg::with_name("SEED")
             .help("String to seed the RNG with. If unsupplied, a random seed is used."))
        .arg(Arg::with_name("width")
             .short("w")
             .validator(check_nat)
             .default_value("1024"))
        .arg(Arg::with_name("height")
             .short("h")
             .validator(check_nat)
             .default_value("768"))
        .arg(Arg::with_name("zoom")
             .short("z")
             .validator(check_float)
             .default_value("1.0"))
        .arg(Arg::with_name("depth")
             .help("Complexity of the generated image.")
             .short("d")
             .validator(check_nat)
             .default_value("6"))
        .get_matches();

    let width = args.value_of("width").unwrap().parse().unwrap();
    let height = args.value_of("height").unwrap().parse().unwrap();
    let zoom = args.value_of("zoom").unwrap().parse().unwrap();
    let depth = args.value_of("depth").unwrap().parse().unwrap();

    let file = File::create(Path::new(args.value_of_os("FILE").unwrap())).expect("couldn't open output file");
    let mut encoder = png::Encoder::new(BufWriter::new(file), width, height);
    encoder.set(png::ColorType::RGB).set(png::BitDepth::Eight);
    let mut encoder = encoder.write_header().expect("couldn't write png header");

    let seed = if let Some(text) = args.value_of("SEED") {
        let mut hasher = Blake2b::default();
        hasher.input(text.as_bytes());
        let x = hasher.result();
        unsafe { *(x.as_ptr() as *const [u32; 4]) }
    } else {
        rand::random()
    };

    let mut rng = XorShiftRng::from_seed(seed);
    let mut data = vec![Color([0.0; 3]); (width*height) as usize];
    let expr = generate(&mut rng, depth);
    println!("generating {}", expr);
    render(expr, &mut data, width as usize, height as usize, zoom);

    println!("encoding");
    let data = data.iter().map(|&Color(x)| [convert(x[0]), convert(x[1]), convert(x[2])]).collect::<Vec<[u8; 3]>>();

    encoder.write_chunk(*b"sRGB", &[0]).unwrap();
    encoder.write_image_data(flatten(&data)).expect("couldn't write image data");
}

fn flatten(xs: &[[u8; 3]]) -> &[u8] {
    unsafe { std::slice::from_raw_parts(xs.as_ptr() as *const u8, xs.len() * 3) }
}

fn convert(x: f32) -> u8 {
    let x = if x <= 0.0031308 { 12.92 * x } else { 1.055 * x.powf(1.0 / 2.4) - 0.055 };
    let x = if x <= 0.0 { 0.0 } else if x >= 1.0 { 1.0 } else { x };
    (x * 255.0) as u8
}

fn render(e: Expr, out: &mut [Color], width: usize, height: usize, zoom: f32) {
    let aspect = width as f32 / height as f32;
    for py in 0..height {
        let y = zoom * (2.0 * (py as f32 / (height - 1) as f32) - 1.0);
        for px in 0..width {
            let x = zoom * aspect * (2.0 * (px as f32 / (width - 1) as f32) - 1.0);
            out[px + py * width] = e.eval(na::Point2::new(x, y));
        }
    }
}

fn generate<R: Rng>(rng: &mut R, depth: u32) -> Expr {
    use Expr::*;
    if depth == 0 {
        match Range::new(0, 2).ind_sample(rng) {
            0 => Constant(rng.gen()),
            1 => Radial,
            _ => unreachable!(),
        }
    } else {
        match Range::new(0, 4).ind_sample(rng) {
            0 => {
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
                Transform(Box::new(generate(rng, depth - 1)), xf)
            }
            1 => Multiply(Box::new(generate(rng, depth - 1)), Box::new(generate(rng, depth - 1))),
            2 => Invert(Box::new(generate(rng, depth - 1))),
            3 => Tile(Box::new(generate(rng, depth - 1))),
            _ => unreachable!(),
        }
    }
}
