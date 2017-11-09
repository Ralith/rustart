extern crate clap;
extern crate indicatif;
extern crate blake2;
extern crate rand;
extern crate png;

use std::fs::File;
use std::path::Path;
use std::io::BufWriter;
use clap::{Arg, App};
use blake2::{Blake2b, Digest};
use rand::{Rng, SeedableRng, XorShiftRng};
use png::HasParameters;

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
             .help("File to write PNG output to")
             .required(true))
        .arg(Arg::with_name("SEED")
             .help("String to seed the RNG with. If unsupplied, a random seed is used."))
        .arg(Arg::with_name("width")
             .short("w")
             .validator(check_nat)
             .default_value("640"))
        .arg(Arg::with_name("height")
             .short("h")
             .validator(check_nat)
             .default_value("480"))
        .arg(Arg::with_name("zoom")
             .short("z")
             .validator(check_float)
             .default_value("1.0"))
        .get_matches();

    let width = args.value_of("width").unwrap().parse().unwrap();
    let height = args.value_of("height").unwrap().parse().unwrap();
    let zoom = args.value_of("zoom").unwrap().parse().unwrap();

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
        rand::thread_rng().gen()
    };

    let mut rng = XorShiftRng::from_seed(seed);
    let mut data = vec![[0.0; 3]; (width*height) as usize];
    render(&mut data, &mut rng, width as usize, height as usize, zoom);

    let data = data.iter().map(|x| [clamp(x[0]), clamp(x[1]), clamp(x[2])]).collect::<Vec<[u8; 3]>>();

    encoder.write_image_data(flatten(&data)).expect("couldn't write image data");
}

fn flatten(xs: &[[u8; 3]]) -> &[u8] {
    unsafe { std::slice::from_raw_parts(xs.as_ptr() as *const u8, xs.len() * 3) }
}

fn clamp(x: f32) -> u8 {
    let x = if x <= 0.0 { 0.0 } else if x >= 1.0 { 1.0 } else { x };
    (x * 255.0) as u8
}

fn render<R: Rng>(out: &mut [[f32; 3]], mut rng: R, width: usize, height: usize, zoom: f32) {
    let aspect = width as f32 / height as f32;
    for py in 0..height {
        let y = zoom * (2.0 * (py as f32 / (height - 1) as f32) - 1.0);
        for px in 0..width {
            let x = zoom * aspect * (2.0 * (px as f32 / (width - 1) as f32) - 1.0);
            let v = (x.powi(2) + y.powi(2)).sqrt();
            out[px + py * width] = [v, v, v];
        }
    }
}
