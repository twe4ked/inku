# inku

An RGBA [`Color`] backed by a `u32`.

## Examples

```rust
type RGBA = inku::Color<inku::RGBA>;

let color = RGBA::new(0x000000ff);
let new_color = color
    // Lighten the color by 10%
    .lighten(0.1)
    // Saturate the color by 30%
    .saturate(0.3);

assert_eq!(new_color.to_u32(), 0x201111ff);

// 4 bytes
assert_eq!(4, std::mem::size_of::<RGBA>());
```

## Storage Formats

An RGBA color backed by a `u32`.

There are multiple storage formats to choose from, [`ZRGB`] and [`RGBA`]. These determine how
the underlying `u32` is laid out.

```rust
type RGBA = inku::Color<inku::RGBA>;
type ZRGB = inku::Color<inku::ZRGB>;

assert_eq!(RGBA::new(0xfacadeff).to_u32(), 0xfacadeff);

// NOTE: The high byte is zeroed out
assert_eq!(ZRGB::new(0xfffacade).to_u32(), 0x00facade);
```

## Manipulations are lossy

Because we're representing the colour with a `u32`, manipulations are not reversible.
Consider the following:

```rust
type RGBA = inku::Color<inku::RGBA>;
let color = RGBA::new(0xfacadeff);

// We convert the RGB values to HSL and desaturated the color
let desaturated_color = color.desaturate(0.1);
assert_eq!(0xf7ccdeff, desaturated_color.to_u32());

// We don't know what our original hue was, so we can't get back to the original color
let resaturated_color = desaturated_color.saturate(0.1);
assert_eq!(0xf9c9ddff, resaturated_color.to_u32());
```

License: MIT OR Apache-2.0
