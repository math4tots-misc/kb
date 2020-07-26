use crate::rterr;
use crate::Val;
use crate::Color;
use crate::Event;
use sdl2::pixels::Color as SdlColor;
use sdl2::event::Event as SdlEvent;

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

pub(super) fn conve(event: SdlEvent) -> Result<Option<Event>, Val> {
    match event {
        SdlEvent::Quit { timestamp: _ } => {
            // TODO: In the future make this configurable
            // For now, we always throw on a quit event, so that
            // there's a way to exit the application even if you forget
            Err(rterr("Quit"))
        }
        SdlEvent::TextInput {
            timestamp: _,
            window_id: _,
            text,
        } => {
            Ok(Some(Event::Text(text)))
        }
        SdlEvent::KeyDown {
            timestamp: _,
            window_id: _,
            keycode,
            scancode: _,
            keymod: _,
            repeat,
        } => {
            Ok(keycode.map(|keycode| {
                let keycode = format!("{:?}", keycode);
                if repeat {
                    Event::KeyRepeat(keycode)
                } else {
                    Event::KeyDown(keycode)
                }
            }))
        }
        SdlEvent::KeyUp {
            timestamp: _,
            window_id: _,
            keycode,
            scancode: _,
            keymod: _,
            repeat: _,
        } => {
            Ok(keycode.map(|keycode| Event::KeyUp(format!("{:?}", keycode))))
        }
        _ => Ok(None)
    }
}
