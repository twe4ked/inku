type RGBA = inku::Color<inku::RGBA>;
type ZRGB = inku::Color<inku::ZRGB>;

const RED: fn(u8, u8, u8, u8) -> (u8, u8, u8, u8) = |_r, g, b, a| (255, g, b, a);

fn main() {
    let mut color = RGBA::new(0x01020304);
    eprintln!("Initial color\t{:?}", color);

    color = color.lighten(0.5);
    eprintln!("Lighten 0.5\t{:?}", color);

    color = color.map(RED);
    eprintln!("Max red channel\t{:?}", color);

    for _ in 0..3 {
        color = color.desaturate(0.3);
        eprintln!("Desaturate 0.3\t{:?}", color);
    }

    for _ in 0..3 {
        color = color.saturate(0.3);
        eprintln!("Saturate 0.3\t{:?}", color);
    }

    for _ in 0..3 {
        color = color.rotate_hue(45.0);
        eprintln!("Rotate hue 45.0\t{:?}", color);
    }

    color = color.darken(0.5);
    eprintln!("Darken 0.5\t{:?}", color);

    let mut color = ZRGB::from(<(u8, u8, u8)>::from(color));
    eprintln!("ZRGB\t\t{:?}", color);

    color = color.lighten(0.6);
    eprintln!("Lighten 0.5\t{:?}", color);

    for _ in 0..3 {
        color = color.rotate_hue(-90.0);
        eprintln!("Rotate hue 45.0\t{:?}", color);
    }
}
