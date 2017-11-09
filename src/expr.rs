use std::ops;
use std::iter;
use std::fmt;

use rand::{Rand, Rng, Closed01};
use na;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Constant(Color),
    Radial,
    Square,
    Cos,
    Transform(Box<Expr>, na::Affine2<f32>),
    Multiply(Box<Expr>, Box<Expr>),
    Average(Vec<Expr>),
    Invert(Box<Expr>),
    Tile(Box<Expr>),
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
            Square => Color::gray((p.x.abs() + p.y.abs()) / 2.0),
            Cos => Color::gray((p.x.cos() + 1.0) / 2.0),
            Transform(ref e, ref xf) => e.eval(xf * p),
            Multiply(ref e, ref f) => e.eval(p) * f.eval(p),
            Average(ref es) => es.iter().map(|e| e.eval(p)).sum::<Color>() / (es.len() as f32),
            Invert(ref e) => -e.eval(p),
            Tile(ref e) => e.eval(na::Point2::new(wrap(p.x, -1.0, 1.0),
                                                  wrap(p.y, -1.0, 1.0)))
        }
    }

    pub fn simplify(&self) -> Expr {
        use self::Expr::*;
        match *self {
            Transform(ref e, ref xf) => Transform(Box::new(e.simplify()), *xf),
            Multiply(ref e, ref f) => Multiply(Box::new(e.simplify()), Box::new(f.simplify())),
            Invert(ref e) => match **e {
                ref f@Invert(_) => match f.simplify() { Invert(x) => *x, x => Invert(Box::new(x)) },
                ref x => Invert(Box::new(x.simplify())),
            }
            Tile(ref e) => match **e {
                Constant(c) => Constant(c),
                ref e => Tile(Box::new(e.simplify())),
            }
            Average(ref es) => Average(es.iter().flat_map(|e| match *e {
                ref e@Average(_) => match e.simplify() { Average(es) => es.into_iter(), e => vec![e].into_iter() },
                ref e => vec![e.simplify()].into_iter(),
            }).collect()),
            ref x => x.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Expr::*;

    #[test]
    fn simplify_invert() {
        assert_eq!(Invert(Box::new(Invert(Box::new(Invert(Box::new(Invert(Box::new(Invert(Box::new(Radial)))))))))).simplify(),
                   Invert(Box::new(Radial)));
    }
}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Constant(x) => x.fmt(f),
            Radial => f.pad("radial"),
            Square => f.pad("square"),
            Cos => f.pad("cos"),
            Transform(ref e, _) => write!(f, "xf({})", e),
            Multiply(ref e, ref g) => write!(f, "mul({}, {})", e, g),
            Average(ref es) => {
                let mut iter = es.iter();
                f.write_str("avg(")?;
                iter.next().unwrap().fmt(f)?;
                while let Some(e) = iter.next() {
                    f.write_str(", ")?;
                    e.fmt(f)?;
                }
                f.write_str(")")?;
                Ok(())
            }
            Invert(ref e) => write!(f, "-{}", e),
            Tile(ref e) => write!(f, "tile({})", e),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Color(pub [f32; 3]);

impl Color {
    fn gray(x: f32) -> Color { Color([x; 3]) }
}

impl ops::Mul for Color {
    type Output = Color;
    fn mul(self, rhs: Color) -> Color { Color([self.0[0] * rhs.0[0], self.0[1] * rhs.0[1], self.0[2] * rhs.0[2]]) }
}

impl ops::Add for Color {
    type Output = Color;
    fn add(self, rhs: Color) -> Color { Color([self.0[0] + rhs.0[0], self.0[1] + rhs.0[1], self.0[2] + rhs.0[2]]) }
}

impl ops::Div<f32> for Color {
    type Output = Color;
    fn div(self, rhs: f32) -> Color { Color([self.0[0] / rhs, self.0[1] / rhs, self.0[2] / rhs]) }
}

impl ops::Neg for Color {
    type Output = Color;
    fn neg(self) -> Color { Color([1.0 - self.0[0], 1.0 - self.0[1], 1.0 - self.0[2]]) }
}

impl iter::Sum for Color {
    fn sum<I>(iter: I) -> Color
        where I: Iterator<Item=Color>
    {
        iter.fold(Color::gray(0.0), |a, x| a + x)
    }
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
