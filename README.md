# inku

An RGB color backed by a `u32`.

Example:

```rust
type Color = inku::Color<inku::ZRGB>;

let color = Color::new(0x000000);
let new_color = color
    // Lighten the color by 10%
    .lighten(0.1)
    // Saturate the color by 30%
    .saturate(0.3);

assert_eq!(new_color.to_u32(), 0x201111);
```

## Manipulations are lossy

Because we're representing the colour with a `u32`, manipulations are not reversible.
Consider the following:

```rust
let color = Color::new(0xfacade);

// We convert the RGB values to HSL and desaturated the color
let desaturated_color = color.desaturate(0.1);
assert_eq!(0xf7ccde, desaturated_color.to_u32());

// We don't know what our original hue was, so we can't get back to the original color
let resaturated_color = desaturated_color.saturate(0.1);
assert_eq!(0xf9c9dd, resaturated_color.to_u32());
```

License: MIT OR Apache-2.0
