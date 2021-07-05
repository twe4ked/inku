#![allow(clippy::many_single_char_names)]

//! An RGB color backed by a `u32`.
//!
//! Example:
//!
//! ```
//! use inku::Color;
//!
//! let color = Color::new(0x000000);
//! let new_color = color
//!     // Lighten the color by 10%
//!     .lighten(0.1)
//!     // Saturate the color by 30%
//!     .saturate(0.3);
//!
//! assert_eq!(new_color.to_u32(), 0x201111);
//! ```
//!
//! # Manipulations are lossy
//!
//! Because we're representing the colour with a `u32`, manipulations are not reversible.
//! Consider the following:
//!
//! ```
//! # use inku::Color;
//! let color = Color::new(0xfacade);
//!
//! // We convert the RGB values to HSL and desaturated the color
//! let desaturated_color = color.desaturate(0.1);
//! assert_eq!(0xf7ccde, desaturated_color.to_u32());
//!
//! // We don't know what our original hue was, so we can't get back to the original color
//! let resaturated_color = desaturated_color.saturate(0.1);
//! assert_eq!(0xf9c9dd, resaturated_color.to_u32());
//! ```

use std::fmt;

/// An RGB color backed by a `u32`.
///
/// Example:
///
/// ```
/// use inku::Color;
///
/// let color = Color::new(0x000000);
/// // Lighten the color by 10%
/// let lighter_color = color.lighten(0.1);
/// assert_eq!(lighter_color.to_u32(), 0x191919);
/// ```
#[derive(Copy, Clone, PartialEq, Default, Hash)]
pub struct Color(u32);

impl Color {
    /// Initializes a new `Color` from a `u32`.
    ///
    /// The `u32` is treated as follows:
    ///
    /// ```text
    /// 0x00000000
    ///   ^^ ignored
    ///     ^^ red
    ///       ^^ green
    ///         ^^ blue
    /// ```
    #[must_use]
    pub const fn new(color: u32) -> Self {
        Self(color)
    }

    /// Lightens the color by translating to HSL color space then adjusting the lightness value.
    ///
    /// # Panics
    ///
    /// Panics if `percent` is not between `0.0` and `1.0`
    #[must_use]
    pub fn lighten(self, percent: f64) -> Self {
        assert_percent(percent);

        // First convert to HSL
        let (h, s, mut l) = self.to_hsl();

        // Increase the lightness and ensure we don't go over 100.0
        l = (l + percent * 100.0).min(100.0);

        Color::from_hsl(h, s, l)
    }

    /// Darkens the color by translating to HSL color space then adjusting the lightness value.
    ///
    /// # Panics
    ///
    /// Panics if `percent` is not between `0.0` and `1.0`
    #[must_use]
    pub fn darken(self, percent: f64) -> Self {
        assert_percent(percent);

        // First convert to HSL
        let (h, s, mut l) = self.to_hsl();

        // Decrease the lightness and ensure we don't go under 0.0
        l = (l - percent * 100.0).max(0.0);

        Color::from_hsl(h, s, l)
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

        // First convert to HSL
        let (h, mut s, l) = self.to_hsl();

        // Increase the saturation and ensure we don't go over 100.0
        s = (s + percent * 100.0).min(100.0);

        Color::from_hsl(h, s, l)
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

        // First convert to HSL
        let (h, mut s, l) = self.to_hsl();

        // Decrease the saturation and ensure we don't go under 0.0
        s = (s - percent * 100.0).max(0.0);

        Color::from_hsl(h, s, l)
    }

    /// Rotate the hue by translating to HSL color space then adjusting the hue value. Takes a
    /// value between `0.0` and `360.0`.
    ///
    /// # Panics
    ///
    /// Panics if `amount` is less than `0.0`
    #[must_use]
    pub fn rotate_hue(self, amount: f64) -> Self {
        assert!(amount >= 0.0, "amount must be greater than 0.0");

        // First convert to HSL
        let (mut h, s, l) = self.to_hsl();

        // Add the amount and ensure the value is a positive number between 0.0 and 360.0
        h = ((h + amount) % 360.0 + 360.0) % 360.0;

        Color::from_hsl(h, s, l)
    }

    /// Returns the underlying u32.
    pub fn to_u32(self) -> u32 {
        self.0
    }

    /// The [percieved brightness](https://www.w3.org/TR/AERT#color-contrast) of the color (a
    /// number between `0.0` and `1.0`).
    pub fn brightness(self) -> f64 {
        let (r, g, b) = self.to_rgb();
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

    fn to_rgb(self) -> (u8, u8, u8) {
        let r = (self.0 >> 16) & 0xff;
        let g = (self.0 >> 8) & 0xff;
        let b = self.0 & 0xff;
        (r as u8, g as u8, b as u8)
    }

    fn from_rgb(r: u8, g: u8, b: u8) -> Self {
        let r = r as u32;
        let g = g as u32;
        let b = b as u32;
        Self((r << 16) | (g << 8) | b)
    }

    fn to_hsl(self) -> (f64, f64, f64) {
        let (r, g, b) = self.to_rgb();
        rgb_to_hsl(r, g, b)
    }

    fn from_hsl(h: f64, s: f64, l: f64) -> Self {
        let (r, g, b) = hsl_to_rgb(h, s, l);
        Self::from_rgb(r as u8, g as u8, b as u8)
    }
}

#[cfg(not(test))]
impl fmt::Debug for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Color({:#08x})", &self.0 & 0xffffff)
    }
}

impl From<u32> for Color {
    fn from(color: u32) -> Self {
        Self::new(color)
    }
}

impl From<(u8, u8, u8)> for Color {
    fn from(rgb: (u8, u8, u8)) -> Self {
        let (r, g, b) = rgb;
        Self::from_rgb(r, g, b)
    }
}

impl From<Color> for u32 {
    fn from(color: Color) -> u32 {
        color.to_u32()
    }
}

impl From<Color> for (u8, u8, u8) {
    fn from(color: Color) -> (u8, u8, u8) {
        color.to_rgb()
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
fn hsl_to_rgb(h: f64, mut s: f64, mut l: f64) -> (u8, u8, u8) {
    debug_assert!(
        (0.0..=360.0).contains(&h),
        "h must be between 0.0 and 360.0"
    );
    debug_assert!(
        (0.0..=100.0).contains(&s),
        "s must be between 0.0 and 100.0"
    );
    debug_assert!(
        (0.0..=100.0).contains(&l),
        "l must be between 0.0 and 100.0"
    );

    // Since we'll use a range of 0.0-100.0 for the saturation and lightness, the first step is to
    // divide them by 100.0 to values between 0.0 and 1.0.
    s /= 100.0;
    l /= 100.0;

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

    // To get the final RGB value, we add m to each channel, multiply it by 255
    r = (r + m) * 255.0;
    g = (g + m) * 255.0;
    b = (b + m) * 255.0;

    (r as u8, g as u8, b as u8)
}

// https://css-tricks.com/converting-color-spaces-in-javascript/#rgb-to-hsl
fn rgb_to_hsl(r: u8, g: u8, b: u8) -> (f64, f64, f64) {
    // First, we must divide the red, green, and blue by 255 to use values between 0.0 and 1.0.
    let r = r as f64 / 255.0;
    let g = g as f64 / 255.0;
    let b = b as f64 / 255.0;

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
    let mut l = (c_max + c_min) / 2.0;

    // Calculate saturation
    let mut s = if delta == 0.0 {
        0.0
    } else {
        // For rounding issues we need to ensure we stay below 1.0
        (delta / (1.0 - (2.0 * l - 1.0).abs())).min(1.0)
    };

    // Multiply l and s by 100
    s *= 100.0;
    l *= 100.0;

    debug_assert!(s <= 100.0);
    debug_assert!(l <= 100.0);

    (h, s, l)
}

#[cfg(test)]
mod tests {
    use super::*;

    impl fmt::Debug for Color {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            use crossterm::style;

            let fg = if self.is_light() {
                style::Color::Black
            } else {
                style::Color::White
            };
            let (r, g, b) = self.to_rgb();
            let bg = style::Color::Rgb { r, g, b };

            write!(
                f,
                "Color({}{}{:#08x}{})",
                style::SetForegroundColor(fg),
                style::SetBackgroundColor(bg),
                self.to_u32() & 0xffffff,
                style::ResetColor,
            )
        }
    }

    #[test]
    fn lighten() {
        let color = Color::new(0x000000);

        for percent in [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0].iter() {
            eprintln!("{:?}", color.lighten(*percent));
        }

        assert_eq!(Color::new(0x191919), color.lighten(0.1));
        assert_eq!(Color::new(0x7f7f7f), color.lighten(0.5));
        assert_eq!(Color::new(0xffffff), color.lighten(1.0));

        assert_eq!(Color::new(0xffffff), Color::new(0xffffff).lighten(1.0));
    }

    #[test]
    fn darken() {
        let color = Color::new(0xffffff);

        for percent in [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0].iter() {
            eprintln!("{:?}", color.darken(*percent));
        }

        assert_eq!(Color::new(0xe5e5e5), color.darken(0.1));
        assert_eq!(Color::new(0x7f7f7f), color.darken(0.5));
        assert_eq!(Color::new(0x000000), color.darken(1.0));

        assert_eq!(Color::new(0x000000), Color::new(0x000000).darken(1.0));
    }

    #[test]
    fn saturate() {
        let color = Color::new(0xe2e2e2);

        for percent in [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0].iter() {
            eprintln!("{:?}", color.saturate(*percent));
        }

        assert_eq!(Color::new(0xe4dfdf), color.saturate(0.1));
        assert_eq!(Color::new(0xf0d3d3), color.saturate(0.5));
        assert_eq!(Color::new(0xffc4c4), color.saturate(1.0));
    }

    #[test]
    fn desaturate() {
        let color = Color::new(0xffc4c4);

        for percent in [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0].iter() {
            eprintln!("{:?}", color.desaturate(*percent));
        }

        assert_eq!(Color::new(0xfcc6c6), color.desaturate(0.1),);
        assert_eq!(Color::new(0xf0d2d2), color.desaturate(0.5));
        assert_eq!(Color::new(0xe1e1e1), color.desaturate(1.0));
    }

    #[test]
    fn rotate_hue() {
        let color = Color::new(0xffc4c4);

        for n in (0..360).into_iter().step_by(10) {
            eprintln!("{:?}", color.rotate_hue(n as f64));
        }

        assert_eq!(color.rotate_hue(100.0), Color::new(0xd7fec3));
        assert_eq!(color.rotate_hue(200.0), Color::new(0xc3ebfe));
        assert_eq!(color.rotate_hue(300.0), Color::new(0xfec3fe));
    }

    #[test]
    fn to_rgb() {
        assert_eq!((0xaa, 0xbb, 0xcc), Color::new(0xaabbcc).to_rgb());
    }

    #[test]
    fn from_rgb() {
        assert_eq!(Color::new(0xaabbcc), Color::from_rgb(0xaa, 0xbb, 0xcc));
    }

    #[test]
    fn to_hsl() {
        let color = Color::new(0x966432);
        let (h, s, l) = color.to_hsl();

        let assert_float = |a: f64, b: f64| {
            let error_margin = std::f64::EPSILON;
            assert!((a - b).abs() < error_margin, "{:?} == {:?}", a, b)
        };

        assert_float(29.999999999999996, h);
        assert_float(50.000000000000014, s);
        assert_float(39.215686274509810, l);

        // Ensure saturation is within 0.0 and 100.0
        Color::new(0xff2009).to_hsl();
    }

    #[test]
    fn brightness() {
        assert_eq!(0.0, Color::new(0x000000).brightness());
        assert_eq!(1.0, Color::new(0xffffff).brightness());
    }

    #[test]
    fn is_light() {
        let light = Color::new(0xffffff);
        let dark = Color::new(0x000000);

        assert!(light.is_light());
        assert!(!dark.is_light());
    }
}
