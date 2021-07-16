type RGBA = inku::Color<inku::RGBA>;
type ZRGB = inku::Color<inku::ZRGB>;

const RED: fn(u8, u8, u8, u8) -> (u8, u8, u8, u8) = |_r, g, b, a| (255, g, b, a);

fn main() {
    eprintln!("Initial color");
    let mut color = RGBA::new(0x01020304);
    eprintln!("{:?}", color);

    eprintln!("Lighten 0.5");
    color = color.lighten(0.5);
    eprintln!("{:?}", color);

    eprintln!("Fully increase red channel");
    color = color.map(RED);
    eprintln!("{:?}", color);

    eprintln!("Desaturate");
    for _ in 0..3 {
        color = color.desaturate(0.3);
        eprintln!("{:?}", color);
    }

    eprintln!("Saturate");
    for _ in 0..3 {
        color = color.saturate(0.3);
        eprintln!("{:?}", color);
    }

    eprintln!("Rotate hue");
    for _ in 0..3 {
        color = color.rotate_hue(45.0);
        eprintln!("{:?}", color);
    }

    eprintln!("Darken");
    color = color.darken(0.5);
    eprintln!("{:?}", color);

    eprintln!("ZRGB");
    let color = ZRGB::from(<(u8, u8, u8)>::from(color));
    eprintln!("{:?}", color);
}
