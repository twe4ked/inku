#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|input: (u32, f64, f64)| {
    let (c, mut h, mut x) = input;

    h = if h.is_nan() { 0.0 } else { h.clamp(0.0, 360.0) };
    x = if x.is_nan() { 0.0 } else { x.clamp(0.0, 1.0) };

    type RGBA = inku::Color<inku::RGBA>;

    let _ = RGBA::new(c)
        .rotate_hue(h)
        .lighten(x)
        .darken(x)
        .saturate(x)
        .desaturate(x);
});
