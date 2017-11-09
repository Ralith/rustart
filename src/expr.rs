use std::ops;
use std::fmt;

use rand::{Rand, Rng, Closed01};
use na;

#[derive(Debug, Clone)]
pub enum Expr {
    Constant(Color),
    Radial,
    Transform(Box<Expr>, na::Affine2<f32>),
    Multiply(Box<Expr>, Box<Expr>),
    Invert(Box<Expr>),
    Tile(Box<Expr>),
}

fn to_unit_square(p: na::Point2<f32>) -> na::Point2<f32> {
    na::Point2::new((p.x + 1.0) / 2.0, (p.y + 1.0) / 2.0)
}

fn from_unit_square(p: na::Point2<f32>) -> na::Point2<f32> {
    na::Point2::new((p.x * 2.0) - 1.0, (p.y * 2.0) - 1.0)
}

fn wrap(x: f32, min: f32, max: f32) -> f32 {
    let t = (x-min) % (max-min);
    if t < 0.0 { t + max } else { t + min }
}

impl Expr {
    pub fn eval(&self, p: na::Point2<f32>) -> Color {
        use self::Expr::*;
        match *self {
            Constant(x) => x,
            Radial => Color::gray((p.x.powi(2) + p.y.powi(2)).sqrt()),
            Transform(ref e, ref xf) => e.eval(xf * p),
            Multiply(ref e, ref f) => e.eval(p) * f.eval(p),
            Invert(ref e) => -e.eval(p),
            Tile(ref e) => e.eval(na::Point2::new(wrap(p.x, -1.0, 1.0),
                                                  wrap(p.y, -1.0, 1.0)))
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Constant(x) => x.fmt(f),
            Radial => f.pad("radial"),
            Transform(ref e, ref xf) => write!(f, "xf({})", e),
            Multiply(ref e, ref g) => write!(f, "{} * {}", e, g),
            Invert(ref e) => write!(f, "-{}", e),
            Tile(ref e) => write!(f, "tile({})", e),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Color(pub [f32; 3]);

impl Color {
    fn gray(x: f32) -> Color { Color([x; 3]) }
}

impl ops::Mul for Color {
    type Output = Color;
    fn mul(self, rhs: Color) -> Color { Color([self.0[0] * rhs.0[0], self.0[1] * rhs.0[1], self.0[2] * rhs.0[2]]) }
}

impl ops::Neg for Color {
    type Output = Color;
    fn neg(self) -> Color { Color([1.0 - self.0[0], 1.0 - self.0[1], 1.0 - self.0[2]]) }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:.2}, {:.2}, {:.2})", self.0[0], self.0[1], self.0[2])
    }
}

impl Rand for Color {
    fn rand<R: Rng>(rng: &mut R) -> Self {
        let Closed01(r) = rng.gen();
        let Closed01(g) = rng.gen();
        let Closed01(b) = rng.gen();
        Color([r, g, b])
    }
}
