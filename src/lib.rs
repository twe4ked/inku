#![allow(clippy::many_single_char_names)]
#![warn(missing_docs)]

//! An RGBA [`Color`] backed by a `u32`.
//!
//! # Examples
//!
//! ```
//! type RGBA = inku::Color<inku::RGBA>;
//!
//! let color = RGBA::new(0x000000ff);
//! let new_color = color
//!     // Lighten the color by 10%
//!     .lighten(0.1)
//!     // Saturate the color by 30%
//!     .saturate(0.3);
//!
//! assert_eq!(new_color.to_u32(), 0x201111ff);
//!
//! // 4 bytes
//! assert_eq!(4, std::mem::size_of::<RGBA>());
//! ```
//!
//! # Storage Formats
//!
//! An RGBA color backed by a `u32`.
//!
//! There are multiple storage formats to choose from, [`ZRGB`] and [`RGBA`]. These determine how
//! the underlying `u32` is laid out.
//!
//! ```
//! type RGBA = inku::Color<inku::RGBA>;
//! type ZRGB = inku::Color<inku::ZRGB>;
//!
//! assert_eq!(RGBA::new(0xfacadeff).to_u32(), 0xfacadeff);
//!
//! // NOTE: The high byte is zeroed out
//! assert_eq!(ZRGB::new(0xfffacade).to_u32(), 0x00facade);
//! ```
//!
//! # Manipulations are lossy
//!
//! Because we're representing the colour with a `u32`, manipulations are not reversible.
//! Consider the following:
//!
//! ```
//! type RGBA = inku::Color<inku::RGBA>;
//! let color = RGBA::new(0xfacadeff);
//!
//! // We convert the RGB values to HSL and desaturated the color
//! let desaturated_color = color.desaturate(0.1);
//! assert_eq!(0xf7ccdeff, desaturated_color.to_u32());
//!
//! // We don't know what our original hue was, so we can't get back to the original color
//! let resaturated_color = desaturated_color.saturate(0.1);
//! assert_eq!(0xf9c9ddff, resaturated_color.to_u32());
//! ```

use std::fmt;
use std::fmt::Write;
use std::marker::PhantomData;

/// An RGBA color backed by a `u32`.
///
/// There are multiple storage formats to choose from, see the [crate level documentation][crate]
/// for more info.
///
/// # Examples
///
/// ```
/// type RGBA = inku::Color<inku::RGBA>;
/// assert_eq!(RGBA::new(0xfacadeff).to_u32(), 0xfacadeff);
///
/// // 4 bytes
/// assert_eq!(4, std::mem::size_of::<RGBA>());
/// ```
#[derive(Copy, Clone, PartialEq, Default, Hash)]
pub struct Color<T: Storage>(u32, PhantomData<T>);

#[doc(hidden)]
pub trait Storage: PartialEq + Copy + Clone + private::Sealed {
    fn init(color: u32) -> u32 {
        color
    }
    fn decode(color: u32) -> (u8, u8, u8, u8);
    fn encode(r: u8, g: u8, b: u8, a: u8) -> u32;
    fn write_hex(w: &mut dyn Write, color: u32) -> fmt::Result {
        write!(w, "{:#010x}", color)
    }
}

mod private {
    // https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed
    pub trait Sealed {}

    impl Sealed for super::ZRGB {}
    impl Sealed for super::RGBA {}
}

fn decode(color: u32) -> (u8, u8, u8, u8) {
    let b1 = (color >> 24) & 0xff;
    let b2 = (color >> 16) & 0xff;
    let b3 = (color >> 8) & 0xff;
    let b4 = color & 0xff;
    (b1 as u8, b2 as u8, b3 as u8, b4 as u8)
}

fn encode(b1: u8, b2: u8, b3: u8, b4: u8) -> u32 {
    let b1 = b1 as u32;
    let b2 = b2 as u32;
    let b3 = b3 as u32;
    let b4 = b4 as u32;
    (b1 << 24) | (b2 << 16) | (b3 << 8) | b4
}

/// ZRGB (0RGB) storage format.
///
/// ```text
/// 0x00000000
///   ^^ ignored (zeroed out)
///     ^^ red
///       ^^ green
///         ^^ blue
/// ```
#[derive(PartialEq, Copy, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub struct ZRGB;

impl Storage for ZRGB {
    fn init(color: u32) -> u32 {
        color & 0xffffff
    }

    fn decode(color: u32) -> (u8, u8, u8, u8) {
        let (_, r, g, b) = decode(color);
        (r, g, b, 0)
    }

    fn encode(r: u8, g: u8, b: u8, _a: u8) -> u32 {
        encode(0, r, g, b)
    }

    fn write_hex(w: &mut dyn Write, color: u32) -> fmt::Result {
        // The high byte is ignored
        write!(w, "{:#08x}", color & 0xffffff)
    }
}

/// RGBA storage format.
///
/// ```text
/// 0x00000000
///   ^^ red
///     ^^ green
///       ^^ blue
///         ^^ alpha
/// ```
#[derive(PartialEq, Copy, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub struct RGBA;

impl Storage for RGBA {
    fn decode(color: u32) -> (u8, u8, u8, u8) {
        decode(color)
    }

    fn encode(r: u8, g: u8, b: u8, a: u8) -> u32 {
        encode(r, g, b, a)
    }
}

impl<T: Storage> Color<T> {
    /// Initializes a new `Color` from a `u32`.
    ///
    /// # Examples
    ///
    /// ```
    /// type Color = inku::Color<inku::ZRGB>;
    /// let color = Color::new(0x000000);
    /// ```
    ///
    /// Using `ZRGB`, the `u32` is treated as follows:
    ///
    /// ```text
    /// 0x00000000
    ///   ^^ ignored (zeroed out)
    ///     ^^ red
    ///       ^^ green
    ///         ^^ blue
    /// ```
    pub fn new(color: u32) -> Self {
        Self(T::init(color), PhantomData)
    }

    /// Lightens the color by translating to HSL color space then adjusting the lightness value.
    ///
    /// # Panics
    ///
    /// Panics if `percent` is not between `0.0` and `1.0`
    #[must_use]
    pub fn lighten(self, percent: f64) -> Self {
        assert_percent(percent);
        self.map_hsla(|h, s, mut l, a| {
            // Increase the lightness and ensure we don't go over 1.0
            l = (l + percent).min(1.0);
            (h, s, l, a)
        })
    }

    /// Darkens the color by translating to HSL color space then adjusting the lightness value.
    ///
    /// # Panics
    ///
    /// Panics if `percent` is not between `0.0` and `1.0`
    #[must_use]
    pub fn darken(self, percent: f64) -> Self {
        assert_percent(percent);
        self.map_hsla(|h, s, mut l, a| {
            // Decrease the lightness and ensure we don't go below 0.0
            l = (l - percent).max(0.0);
            (h, s, l, a)
        })
    }

    /// Increases saturation of the color by translating to HSL color space then adjusting the
    /// saturation value.
    ///
    /// # Panics
    ///
    /// Panics if `percent` is not between `0.0` and `1.0`
    #[must_use]
    pub fn saturate(self, percent: f64) -> Self {
        assert_percent(percent);
        self.map_hsla(|h, mut s, l, a| {
            // Increase the saturation and ensure we don't go over 1.0
            s = (s + percent).min(1.0);
            (h, s, l, a)
        })
    }

    /// Decreases saturation of the color by translating to HSL color space then adjusting the
    /// saturation value.
    ///
    /// # Panics
    ///
    /// Panics if `percent` is not between `0.0` and `1.0`
    #[must_use]
    pub fn desaturate(self, percent: f64) -> Self {
        assert_percent(percent);
        self.map_hsla(|h, mut s, l, a| {
            // Decrease the saturation and ensure we don't go below 0.0
            s = (s - percent).max(0.0);
            (h, s, l, a)
        })
    }

    /// Rotate the hue by translating to HSL color space then adjusting the hue value. Takes a
    /// value between `0.0` and `360.0`.
    #[must_use]
    pub fn rotate_hue(self, amount: f64) -> Self {
        self.map_hsla(|mut h, s, l, a| {
            // Add the amount and ensure the value is a positive number between 0.0 and 360.0
            h = ((h + amount) % 360.0 + 360.0) % 360.0;
            (h, s, l, a)
        })
    }

    /// Returns the underlying `u32`.
    pub fn to_u32(self) -> u32 {
        self.0
    }

    /// The [percieved brightness](https://www.w3.org/TR/AERT#color-contrast) of the color (a
    /// number between `0.0` and `1.0`).
    pub fn brightness(self) -> f64 {
        let (r, g, b, _a) = self.to_rgba();
        let r = r as f64 / 255.0;
        let g = g as f64 / 255.0;
        let b = b as f64 / 255.0;
        (299.0 * r + 587.0 * g + 114.0 * b) / 1_000.0
    }

    /// Determine whether a color is perceived as a light color ([percieved
    /// brightness](https://www.w3.org/TR/AERT#color-contrast) is greater than `0.5`).
    pub fn is_light(self) -> bool {
        self.brightness() > 0.5
    }

    /// Maps `(r1, g1, b1, a1)` to `(r2, g2, b2, a2)` by applying a function to the channels.
    ///
    /// # Examples
    ///
    /// ```
    /// # use inku::{Color, RGBA};
    /// let color = Color::<RGBA>::new(0x00000011);
    /// assert_eq!(
    ///     color.map(|r, g, b, a| (r, g, b, a + 0x22)).to_u32(),
    ///     0x00000033
    /// );
    /// ```
    ///
    /// ```
    /// # use inku::{Color,  ZRGB};
    /// const F: fn(u8, u8, u8, u8) -> (u8, u8, u8, u8) = |r, g, b, a| {
    ///     assert_eq!(r, 0x22);
    ///     assert_eq!(g, 0x33);
    ///     assert_eq!(b, 0x44);
    ///     assert_eq!(a, 0x00);
    ///     (1, 2, 3, 4)
    /// };
    /// let color = Color::<ZRGB>::new(0x11223344);
    /// assert_eq!(color.map(F).to_u32(), 0x00010203);
    /// ```
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(u8, u8, u8, u8) -> (u8, u8, u8, u8),
    {
        let (r, g, b, a) = self.to_rgba();
        let (r, g, b, a) = f(r, g, b, a);
        Self::from_rgba(r, g, b, a)
    }

    fn map_hsla<F>(&self, f: F) -> Self
    where
        F: Fn(f64, f64, f64, f64) -> (f64, f64, f64, f64),
    {
        let (h, s, l, a) = self.to_hsla();
        let (h, s, l, a) = f(h, s, l, a);
        Color::from_hsla(h, s, l, a)
    }

    fn to_rgba(self) -> (u8, u8, u8, u8) {
        let (r, g, b, a) = T::decode(self.0);
        (r, g, b, a)
    }

    fn from_rgba(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self::new(T::encode(r, g, b, a))
    }

    // NOTE: These {to,from}_hsla functions are private because if you're already dealing with
    // colors in HSLA you're probably better off keeping your colors in HSLA so you don't lose
    // fidelity.

    fn to_hsla(self) -> (f64, f64, f64, f64) {
        let (r, g, b, a) = self.to_rgba();
        let (h, s, l, a) = rgba_to_hsla(r, g, b, a);
        (h, s, l, a)
    }

    fn from_hsla(h: f64, s: f64, l: f64, a: f64) -> Self {
        let (r, g, b, a) = hsla_to_rgba(h, s, l, a);
        Self::from_rgba(r as u8, g as u8, b as u8, a as u8)
    }
}

impl<T: Storage> fmt::Debug for Color<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let storage = format!("{}", std::any::type_name::<T>())
            .split("::")
            .last()
            .expect("no type name")
            .to_owned();

        write!(f, "Color<{}>(", storage)?;

        #[cfg(any(test, feature = "crossterm"))]
        {
            use crossterm::style;

            let fg = style::SetForegroundColor(if self.is_light() {
                style::Color::Black
            } else {
                style::Color::White
            });
            let (r, g, b, _a) = self.to_rgba();
            let bg = style::SetBackgroundColor(style::Color::Rgb { r, g, b });

            write!(f, "{}{}", fg, bg)?;
        }

        T::write_hex(f, self.0)?;

        #[cfg(any(test, feature = "crossterm"))]
        write!(f, "{}", crossterm::style::ResetColor)?;

        write!(f, ")")
    }
}

impl<T: Storage> From<u32> for Color<T> {
    fn from(color: u32) -> Self {
        Self::new(color)
    }
}

impl<T: Storage> From<(u8, u8, u8)> for Color<T> {
    fn from(rgb: (u8, u8, u8)) -> Self {
        let (r, g, b) = rgb;
        Self::from_rgba(r, g, b, 0)
    }
}

impl<T: Storage> From<(u8, u8, u8, u8)> for Color<T> {
    fn from(rgba: (u8, u8, u8, u8)) -> Self {
        let (r, g, b, a) = rgba;
        Self::from_rgba(r, g, b, a)
    }
}

impl<T: Storage> From<Color<T>> for u32 {
    fn from(color: Color<T>) -> u32 {
        color.to_u32()
    }
}

impl<T: Storage> From<Color<T>> for (u8, u8, u8) {
    fn from(color: Color<T>) -> (u8, u8, u8) {
        let (r, g, b, _a) = color.to_rgba();
        (r, g, b)
    }
}

impl<T: Storage> From<Color<T>> for (u8, u8, u8, u8) {
    fn from(color: Color<T>) -> (u8, u8, u8, u8) {
        color.to_rgba()
    }
}

fn assert_percent(percent: f64) {
    assert!(
        (0.0..=1.0).contains(&percent),
        "percent ({:?}) must be between 0.0 and 1.0",
        percent
    );
}

// https://css-tricks.com/converting-color-spaces-in-javascript/#hsl-to-rgb
fn hsla_to_rgba(h: f64, s: f64, l: f64, mut a: f64) -> (u8, u8, u8, u8) {
    debug_assert!(
        (0.0..=360.0).contains(&h),
        "h must be between 0.0 and 360.0"
    );
    debug_assert!((0.0..=1.0).contains(&s), "s must be between 0.0 and 1.0");
    debug_assert!((0.0..=1.0).contains(&l), "l must be between 0.0 and 1.0");

    // Next, we find chroma (c), which is color intensity
    let c = (1.0 - (2.0 * l - 1.0).abs()) * s;

    // Then we use x for the second largest component (first being chroma)
    let x = c * (1.0 - ((h / 60.0) % 2.0 - 1.0).abs());

    // Then the amount to add to each channel to match the lightness (m)
    let m = l - c / 2.0;

    // The hue will determine what the red, green, and blue should be depending on which 60 degree
    // sector of the color wheel it lies.
    let (mut r, mut g, mut b) = if (0.0..60.0).contains(&h) {
        (c, x, 0.0)
    } else if (60.0..120.0).contains(&h) {
        (x, c, 0.0)
    } else if (120.0..180.0).contains(&h) {
        (0.0, c, x)
    } else if (180.0..240.0).contains(&h) {
        (0.0, x, c)
    } else if (240.0..300.0).contains(&h) {
        (x, 0.0, c)
    } else if (300.0..360.0).contains(&h) {
        (c, 0.0, x)
    } else {
        unreachable!();
    };

    // To get the final RGBA value, we add m to each channel, multiply it by 255
    r = (r + m) * 255.0;
    g = (g + m) * 255.0;
    b = (b + m) * 255.0;
    a = a * 255.0;

    (r as u8, g as u8, b as u8, a as u8)
}

// https://css-tricks.com/converting-color-spaces-in-javascript/#rgb-to-hsl
fn rgba_to_hsla(r: u8, g: u8, b: u8, a: u8) -> (f64, f64, f64, f64) {
    // First, we must divide the red, green, and blue, and alpha by 255 to use values between 0.0
    // and 1.0.
    let r = r as f64 / 255.0;
    let g = g as f64 / 255.0;
    let b = b as f64 / 255.0;
    let a = a as f64 / 255.0;

    // Then we find the minimum and maximum of those values (c_min and c_max) as well as the
    // difference between them (delta).
    let c_min = r.min(g.min(b));
    let c_max = r.max(g.max(b));
    let delta = c_max - c_min;

    let error_margin = std::f64::EPSILON;

    // Calculate the hue
    let mut h = if delta == 0.0 {
        0.0
    } else if (c_max - r).abs() < error_margin {
        // Red is max
        ((g - b) / delta) % 6.0
    } else if (c_max - g).abs() < error_margin {
        // Green is max
        (b - r) / delta + 2.0
    } else {
        // Blue is max
        (r - g) / delta + 4.0
    };

    h *= 60.0;

    // Make negative hues positive behind 360 degrees
    if h < 0.0 {
        h += 360.0;
    }

    // Calculate lightness
    let l = (c_max + c_min) / 2.0;

    // Calculate saturation
    let s = if delta == 0.0 {
        0.0
    } else {
        // For rounding issues we need to ensure we stay below 1.0
        (delta / (1.0 - (2.0 * l - 1.0).abs())).min(1.0)
    };

    debug_assert!(s <= 1.0);
    debug_assert!(l <= 1.0);

    (h, s, l, a)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lighten() {
        let color = Color::<ZRGB>::new(0x000000);

        for percent in [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0].iter() {
            eprintln!("{:?}", color.lighten(*percent));
        }

        assert_eq!(Color::<ZRGB>::new(0x191919), color.lighten(0.1));
        assert_eq!(Color::<ZRGB>::new(0x7f7f7f), color.lighten(0.5));
        assert_eq!(Color::<ZRGB>::new(0xffffff), color.lighten(1.0));

        assert_eq!(
            Color::<ZRGB>::new(0xffffff),
            Color::<ZRGB>::new(0xffffff).lighten(1.0)
        );
    }

    #[test]
    fn darken() {
        let color = Color::<ZRGB>::new(0xffffff);

        for percent in [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0].iter() {
            eprintln!("{:?}", color.darken(*percent));
        }

        assert_eq!(Color::<ZRGB>::new(0xe5e5e5), color.darken(0.1));
        assert_eq!(Color::<ZRGB>::new(0x7f7f7f), color.darken(0.5));
        assert_eq!(Color::<ZRGB>::new(0x000000), color.darken(1.0));

        assert_eq!(
            Color::<ZRGB>::new(0x000000),
            Color::<ZRGB>::new(0x000000).darken(1.0)
        );
    }

    #[test]
    fn saturate() {
        let color = Color::<ZRGB>::new(0xe2e2e2);

        for percent in [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0].iter() {
            eprintln!("{:?}", color.saturate(*percent));
        }

        assert_eq!(Color::<ZRGB>::new(0xe4dfdf), color.saturate(0.1));
        assert_eq!(Color::<ZRGB>::new(0xf0d3d3), color.saturate(0.5));
        assert_eq!(Color::<ZRGB>::new(0xffc4c4), color.saturate(1.0));
    }

    #[test]
    fn desaturate() {
        let color = Color::<ZRGB>::new(0xffc4c4);

        for percent in [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0].iter() {
            eprintln!("{:?}", color.desaturate(*percent));
        }

        assert_eq!(Color::<ZRGB>::new(0xfcc6c6), color.desaturate(0.1),);
        assert_eq!(Color::<ZRGB>::new(0xf0d2d2), color.desaturate(0.5));
        assert_eq!(Color::<ZRGB>::new(0xe1e1e1), color.desaturate(1.0));
    }

    #[test]
    fn rotate_hue() {
        let color = Color::<ZRGB>::new(0xffc4c4);

        for n in (0..360).into_iter().step_by(10) {
            eprintln!("{:?}", color.rotate_hue(n as f64));
        }

        assert_eq!(color.rotate_hue(100.0), Color::<ZRGB>::new(0xd7fec3));
        assert_eq!(color.rotate_hue(200.0), Color::<ZRGB>::new(0xc3ebfe));
        assert_eq!(color.rotate_hue(300.0), Color::<ZRGB>::new(0xfec3fe));
    }

    #[test]
    fn to_rgba() {
        assert_eq!(
            (0xbb, 0xcc, 0xdd, 0x00),
            Color::<ZRGB>::new(0xaabbccdd).to_rgba()
        );
    }

    #[test]
    fn from_rgba() {
        assert_eq!(
            Color::<ZRGB>::new(0xaabbccdd),
            Color::<ZRGB>::from_rgba(0xbb, 0xcc, 0xdd, 0x00)
        );
    }

    #[test]
    fn to_hsla() {
        let color = Color::<RGBA>::new(0x96643233);
        let (h, s, l, a) = color.to_hsla();

        let assert_float = |a: f64, b: f64| {
            let error_margin = std::f64::EPSILON;
            assert!((a - b).abs() < error_margin, "{:?} == {:?}", a, b)
        };

        assert_float(29.999999999999996, h);
        assert_float(0.50000000000000014, s);
        assert_float(0.39215686274509810, l);
        assert_float(0.2, a);

        // Ensure saturation is within 0.0 and 1.0
        Color::<ZRGB>::new(0xff2009).to_hsla();
    }

    #[test]
    fn brightness() {
        assert_eq!(0.0, Color::<ZRGB>::new(0x000000).brightness());
        assert_eq!(1.0, Color::<ZRGB>::new(0xffffff).brightness());
    }

    #[test]
    fn is_light() {
        let light = Color::<ZRGB>::new(0xffffff);
        let dark = Color::<ZRGB>::new(0x000000);

        assert!(light.is_light());
        assert!(!dark.is_light());
    }

    #[test]
    fn map() {
        let color = Color::<ZRGB>::new(0x00000000);
        assert_eq!(
            color
                .map(|r, g, b, a| (r + 1, g + 2, b + 3, a + 4))
                .to_u32(),
            0x00010203
        );

        let color = Color::<RGBA>::new(0x00000000);
        assert_eq!(
            color
                .map(|r, g, b, a| (r + 1, g + 2, b + 3, a + 4))
                .to_u32(),
            0x01020304
        );

        const RED: fn(u8, u8, u8, u8) -> (u8, u8, u8, u8) = |_r, g, b, a| (255, g, b, a);
        assert_eq!(color.map(RED).to_u32(), 0xff000000);
    }

    #[test]
    fn storage() {
        assert_eq!(0x00bbccdd, ZRGB::init(0xaabbccdd));
        assert_eq!((0xbb, 0xcc, 0xdd, 0x00), ZRGB::decode(0xaabbccdd));
        assert_eq!(0x00bbccdd, ZRGB::encode(0xbb, 0xcc, 0xdd, 0xaa));

        assert_eq!(0xaabbccdd, RGBA::init(0xaabbccdd));
        assert_eq!((0xaa, 0xbb, 0xcc, 0xdd), RGBA::decode(0xaabbccdd));
        assert_eq!(0xaabbccdd, RGBA::encode(0xaa, 0xbb, 0xcc, 0xdd));

        // For sanity checking; cargo test storage -- --nocapture
        eprintln!("{:?}", Color::<ZRGB>::new(0xff_facade));
        eprintln!("{:?}", Color::<RGBA>::new(0xfacade_ff));
    }

    #[test]
    fn test_rgba_to_hsla() {
        let (h, s, l, a) = rgba_to_hsla(1, 2, 3, 4);
        let (r, g, b, a) = hsla_to_rgba(h, s, l, a);

        assert_eq!(r, 1);
        assert_eq!(g, 2);
        assert_eq!(b, 3);
        assert_eq!(a, 4);
    }
}
