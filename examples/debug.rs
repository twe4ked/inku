type ZRGB = inku::Color<inku::ZRGB>;
type RGBA = inku::Color<inku::RGBA>;

fn main() {
    // Sanity check debug output
    eprintln!("{:?}", ZRGB::new(0xaabbccdd));
    eprintln!("{:?}", RGBA::new(0xaabbccdd));
}
