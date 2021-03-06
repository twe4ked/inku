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
//! // We convert the RGBA values to HSLA and desaturated the color
//! let desaturated_color = color.desaturate(0.1);
//! assert_eq!(0xf7ccdeff, desaturated_color.to_u32());
//!
//! // We don't know what our original hue was, so we can't get back to the original color
//! let resaturated_color = desaturated_color.saturate(0.1);
//! assert_eq!(0xf9c9ddff, resaturated_color.to_u32());
//! ```
//!
//! # Crossterm feature
//!
//! The `crossterm` feature improves the `Debug` impl by printing the color using the `crossterm`
//! crate.

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

/// Used to specify the way the RGBA channels are packed into a `u32`.
///
/// Storage is a [sealed trait] because don't expect there to be many useful formats to implement.
/// Please open a PR if a format you wish to use is missing.
///
/// [sealed trait]: https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed
pub trait Storage: PartialEq + Copy + Clone + private::Sealed {
    /// Run before storing a "raw" `u32`.
    fn from_raw(color: u32) -> u32 {
        color
    }

    /// Unpack a `u32` to an array of `u8`s in RGBA order.
    fn to_rgba(color: u32) -> [u8; 4];

    /// Pack an array of `u8`s in RGBA order into a `u32`.
    fn from_rgba(color: [u8; 4]) -> u32;

    /// Write out the `u32` for debugging.
    fn write_debug(w: &mut dyn Write, color: u32) -> fmt::Result {
        write!(w, "{:#010x}", color)
    }
}

mod private {
    pub trait Sealed {}

    impl Sealed for super::ZRGB {}
    impl Sealed for super::RGBA {}
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
    fn from_raw(color: u32) -> u32 {
        color & 0xffffff
    }

    #[inline]
    fn to_rgba(color: u32) -> [u8; 4] {
        let [_, r, g, b] = color.to_be_bytes();
        [r, g, b, 0]
    }

    #[inline]
    fn from_rgba(color: [u8; 4]) -> u32 {
        let [r, g, b, _] = color;
        u32::from_be_bytes([0, r, g, b])
    }

    fn write_debug(w: &mut dyn Write, color: u32) -> fmt::Result {
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
    #[inline]
    fn to_rgba(color: u32) -> [u8; 4] {
        color.to_be_bytes()
    }

    #[inline]
    fn from_rgba(color: [u8; 4]) -> u32 {
        u32::from_be_bytes(color)
    }
}

impl<T: Storage> Color<T> {
    /// Initializes a new `Color` from a `u32`.
    ///
    /// # Examples
    ///
    /// Using [`ZRGB`] storage:
    ///
    /// ```
    /// type Color = inku::Color<inku::ZRGB>;
    ///
    /// let color = Color::new(0x11223344);
    /// assert_eq!(0x223344, color.to_u32());
    /// ```
    ///
    /// Using [`RGBA`] storage:
    ///
    /// ```
    /// type Color = inku::Color<inku::RGBA>;
    ///
    /// let color = Color::new(0x11223344);
    /// assert_eq!(0x11223344, color.to_u32());
    /// ```
    pub fn new(color: u32) -> Self {
        Self(T::from_raw(color), PhantomData)
    }

    /// Lightens the color by translating to HSLA color space then adjusting the lightness value.
    ///
    /// # Panics
    ///
    /// Panics if `percent` is not between `0.0` and `1.0`
    ///
    /// # Examples
    ///
    /// ```
    /// # use inku::{Color, RGBA};
    /// let mut color = Color::<RGBA>::new(0x11223344);
    /// color = color.lighten(0.2);
    /// assert_eq!(Color::<RGBA>::new(0x2a557f44), color);
    /// ```
    #[must_use]
    pub fn lighten(self, percent: f64) -> Self {
        assert_percent(percent);
        self.map_hsla(|[h, s, mut l, a]| {
            // Increase the lightness and ensure we don't go over 1.0
            l = (l + percent).min(1.0);
            [h, s, l, a]
        })
    }

    /// Darkens the color by translating to HSLA color space then adjusting the lightness value.
    ///
    /// # Panics
    ///
    /// Panics if `percent` is not between `0.0` and `1.0`
    ///
    /// # Examples
    ///
    /// ```
    /// # use inku::{Color, RGBA};
    /// let mut color = Color::<RGBA>::new(0x11223344);
    /// color = color.darken(0.2);
    /// assert_eq!(Color::<RGBA>::new(0x00000044), color);
    /// ```
    #[must_use]
    pub fn darken(self, percent: f64) -> Self {
        assert_percent(percent);
        self.map_hsla(|[h, s, mut l, a]| {
            // Decrease the lightness and ensure we don't go below 0.0
            l = (l - percent).max(0.0);
            [h, s, l, a]
        })
    }

    /// Increases saturation of the color by translating to HSLA color space then adjusting the
    /// saturation value.
    ///
    /// # Panics
    ///
    /// Panics if `percent` is not between `0.0` and `1.0`
    ///
    /// # Examples
    ///
    /// ```
    /// # use inku::{Color, RGBA};
    /// let mut color = Color::<RGBA>::new(0x11223344);
    /// color = color.saturate(0.5);
    /// assert_eq!(Color::<RGBA>::new(0x00224344), color);
    /// ```
    #[must_use]
    pub fn saturate(self, percent: f64) -> Self {
        assert_percent(percent);
        self.map_hsla(|[h, mut s, l, a]| {
            // Increase the saturation and ensure we don't go over 1.0
            s = (s + percent).min(1.0);
            [h, s, l, a]
        })
    }

    /// Decreases saturation of the color by translating to HSLA color space then adjusting the
    /// saturation value.
    ///
    /// # Panics
    ///
    /// Panics if `percent` is not between `0.0` and `1.0`
    ///
    /// # Examples
    ///
    /// ```
    /// # use inku::{Color, RGBA};
    /// let mut color = Color::<RGBA>::new(0x11223344);
    /// color = color.desaturate(0.5);
    /// assert_eq!(Color::<RGBA>::new(0x21222244), color);
    /// ```
    #[must_use]
    pub fn desaturate(self, percent: f64) -> Self {
        assert_percent(percent);
        self.map_hsla(|[h, mut s, l, a]| {
            // Decrease the saturation and ensure we don't go below 0.0
            s = (s - percent).max(0.0);
            [h, s, l, a]
        })
    }

    /// Rotate the hue by translating to HSLA color space then adjusting the hue value. Takes a
    /// value between `0.0` and `360.0`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use inku::{Color, RGBA};
    /// let mut color = Color::<RGBA>::new(0x11223344);
    /// color = color.rotate_hue(45.0);
    /// assert_eq!(Color::<RGBA>::new(0x19103344), color);
    /// ```
    #[must_use]
    pub fn rotate_hue(self, amount: f64) -> Self {
        self.map_hsla(|[mut h, s, l, a]| {
            // Add the amount and ensure the value is a positive number between 0.0 and 360.0
            h = ((h + amount) % 360.0 + 360.0) % 360.0;
            [h, s, l, a]
        })
    }

    /// Returns the underlying `u32`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use inku::{Color, RGBA};
    /// assert_eq!(0x11223344, Color::<RGBA>::new(0x11223344).to_u32());
    /// ```
    pub fn to_u32(self) -> u32 {
        self.0
    }

    /// The [percieved brightness](https://www.w3.org/TR/AERT#color-contrast) of the color (a
    /// number between `0.0` and `1.0`).
    ///
    /// # Examples
    ///
    /// ```
    /// # use inku::{Color, RGBA};
    /// assert_eq!(0.2666666666666666, Color::<RGBA>::new(0x444444_ff).brightness());
    /// assert_eq!(0.6, Color::<RGBA>::new(0x999999_ff).brightness());
    /// ```
    pub fn brightness(self) -> f64 {
        let [r, g, b, _a] = self.to_rgba();
        let r = r as f64 / 255.0;
        let g = g as f64 / 255.0;
        let b = b as f64 / 255.0;
        (299.0 * r + 587.0 * g + 114.0 * b) / 1_000.0
    }

    /// Determine whether a color is perceived as a light color ([percieved
    /// brightness](https://www.w3.org/TR/AERT#color-contrast) is greater than `0.5`).
    ///
    /// # Examples
    ///
    /// ```
    /// # use inku::{Color, RGBA};
    /// assert!(!Color::<RGBA>::new(0x444444_ff).is_light());
    /// assert!(Color::<RGBA>::new(0x999999_ff).is_light());
    /// ```
    pub fn is_light(self) -> bool {
        self.brightness() > 0.5
    }

    /// Maps `[u8; 4]` to `[u8; 4]` by applying a function to the channels.
    ///
    /// # Examples
    ///
    /// ```
    /// # use inku::{Color, RGBA};
    /// let color = Color::<RGBA>::new(0x00000011);
    /// assert_eq!(
    ///     color.map_rgba(|[r, g, b, a]| [r, g, b, a + 0x22]).to_u32(),
    ///     0x00000033
    /// );
    /// ```
    ///
    /// Channels are always in RGBA order:
    ///
    /// ```
    /// # use inku::{Color,  ZRGB};
    /// let mut color = Color::<ZRGB>::new(0x11223344);
    /// color = color.map_rgba(|[r, g, b, a]| {
    ///     assert_eq!(r, 0x22);
    ///     assert_eq!(g, 0x33);
    ///     assert_eq!(b, 0x44);
    ///     assert_eq!(a, 0x00);
    ///     [1, 2, 3, 4]
    /// });
    /// assert_eq!(color.to_u32(), 0x00010203);
    /// ```
    pub fn map_rgba<F>(&self, f: F) -> Self
    where
        F: Fn([u8; 4]) -> [u8; 4],
    {
        let [r, g, b, a] = self.to_rgba();
        let [r, g, b, a] = f([r, g, b, a]);
        Self::from_rgba(r, g, b, a)
    }

    fn map_hsla<F>(&self, f: F) -> Self
    where
        F: Fn([f64; 4]) -> [f64; 4],
    {
        let [h, s, l, a] = f(self.to_hsla());
        Color::from_hsla(h, s, l, a)
    }

    fn to_rgba(self) -> [u8; 4] {
        T::to_rgba(self.0)
    }

    fn from_rgba(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self::new(T::from_rgba([r, g, b, a]))
    }

    // NOTE: These {to,from}_hsla functions are private because if you're already dealing with
    // colors in HSLA you're probably better off keeping your colors in HSLA so you don't lose
    // fidelity.

    fn to_hsla(self) -> [f64; 4] {
        let [r, g, b, a] = self.to_rgba();
        rgba_to_hsla(r, g, b, a)
    }

    fn from_hsla(h: f64, s: f64, l: f64, a: f64) -> Self {
        let [r, g, b, a] = hsla_to_rgba(h, s, l, a);
        Self::from_rgba(r as u8, g as u8, b as u8, a as u8)
    }
}

impl<T: Storage> fmt::Debug for Color<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let storage = std::any::type_name::<T>()
            .to_string()
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
            let [r, g, b, _a] = self.to_rgba();
            let bg = style::SetBackgroundColor(style::Color::Rgb { r, g, b });

            write!(f, "{}{}", fg, bg)?;
        }

        T::write_debug(f, self.0)?;

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

impl<T: Storage> From<[u8; 3]> for Color<T> {
    fn from(rgb: [u8; 3]) -> Self {
        let [r, g, b] = rgb;
        Self::from_rgba(r, g, b, 0)
    }
}

impl<T: Storage> From<[u8; 4]> for Color<T> {
    fn from(rgba: [u8; 4]) -> Self {
        let [r, g, b, a] = rgba;
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
        let [r, g, b, _a] = color.to_rgba();
        (r, g, b)
    }
}

impl<T: Storage> From<Color<T>> for (u8, u8, u8, u8) {
    fn from(color: Color<T>) -> (u8, u8, u8, u8) {
        let [r, g, b, a] = color.to_rgba();
        (r, g, b, a)
    }
}

impl<T: Storage> From<Color<T>> for [u8; 3] {
    fn from(color: Color<T>) -> [u8; 3] {
        let [r, g, b, _a] = color.to_rgba();
        [r, g, b]
    }
}

impl<T: Storage> From<Color<T>> for [u8; 4] {
    fn from(color: Color<T>) -> [u8; 4] {
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
fn hsla_to_rgba(h: f64, s: f64, l: f64, mut a: f64) -> [u8; 4] {
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
        panic!("hue is over 360.0");
    };

    // To get the final RGB value, we add m to each channel, multiply it by 255.0
    r = (r + m) * 255.0;
    g = (g + m) * 255.0;
    b = (b + m) * 255.0;

    // Alpha is simply multiplied by 255.0
    a *= 255.0;

    [r as u8, g as u8, b as u8, a as u8]
}

// https://css-tricks.com/converting-color-spaces-in-javascript/#rgb-to-hsl
fn rgba_to_hsla(r: u8, g: u8, b: u8, a: u8) -> [f64; 4] {
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

    [h, s, l, a]
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
            [0xbb, 0xcc, 0xdd, 0x00],
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
        let [h, s, l, a] = color.to_hsla();

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
    fn map_rgba() {
        let color = Color::<ZRGB>::new(0x00000000);
        assert_eq!(
            color
                .map_rgba(|[r, g, b, a]| [r + 1, g + 2, b + 3, a + 4])
                .to_u32(),
            0x00010203
        );

        let color = Color::<RGBA>::new(0x00000000);
        assert_eq!(
            color
                .map_rgba(|[r, g, b, a]| [r + 1, g + 2, b + 3, a + 4])
                .to_u32(),
            0x01020304
        );

        const RED: fn([u8; 4]) -> [u8; 4] = |[_r, g, b, a]| [255, g, b, a];
        assert_eq!(color.map_rgba(RED).to_u32(), 0xff000000);
    }

    #[test]
    fn storage() {
        assert_eq!(0x00bbccdd, ZRGB::from_raw(0xaabbccdd));
        assert_eq!([0xbb, 0xcc, 0xdd, 0x00], ZRGB::to_rgba(0xaabbccdd));
        assert_eq!(0x00bbccdd, ZRGB::from_rgba([0xbb, 0xcc, 0xdd, 0xaa]));

        assert_eq!(0xaabbccdd, RGBA::from_raw(0xaabbccdd));
        assert_eq!([0xaa, 0xbb, 0xcc, 0xdd], RGBA::to_rgba(0xaabbccdd));
        assert_eq!(0xaabbccdd, RGBA::from_rgba([0xaa, 0xbb, 0xcc, 0xdd]));

        // For sanity checking; cargo test storage -- --nocapture
        eprintln!("{:?}", Color::<ZRGB>::new(0xff_facade));
        eprintln!("{:?}", Color::<RGBA>::new(0xfacade_ff));
    }

    #[test]
    fn test_rgba_to_hsla() {
        let [h, s, l, a] = rgba_to_hsla(1, 2, 3, 4);
        let [r, g, b, a] = hsla_to_rgba(h, s, l, a);

        assert_eq!(r, 1);
        assert_eq!(g, 2);
        assert_eq!(b, 3);
        assert_eq!(a, 4);
    }
}
