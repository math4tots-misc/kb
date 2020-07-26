use crate::Color;
use sdl2::pixels::Color as SdlColor;


impl From<Color> for SdlColor {
    fn from(color: Color) -> Self {
        Self::RGBA(
            convf(color.r),
            convf(color.g),
            convf(color.b),
            convf(color.a),
        )
    }
}

fn convf(f: f64) -> u8 {
    let f255 = f * 255.0;
    if f255 <= 0.0 {
        0
    } else if f255 >= 255.0 {
        255
    } else {
        f255 as u8
    }
}
