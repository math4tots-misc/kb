use crate::rterr;
use crate::Color;
use crate::Event;
use crate::Val;
use sdl2::event::Event as SdlEvent;
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
        } => Ok(Some(Event::Text(text.into()))),
        SdlEvent::KeyDown {
            timestamp: _,
            window_id: _,
            keycode,
            scancode: _,
            keymod: _,
            repeat,
        } => Ok(keycode.map(|keycode| {
            let keycode = format!("{:?}", keycode).into();
            if repeat {
                Event::KeyRepeat(keycode)
            } else {
                Event::KeyDown(keycode)
            }
        })),
        SdlEvent::KeyUp {
            timestamp: _,
            window_id: _,
            keycode,
            scancode: _,
            keymod: _,
            repeat: _,
        } => Ok(keycode.map(|keycode| Event::KeyUp(format!("{:?}", keycode).into()))),
        SdlEvent::MouseButtonDown {
            timestamp: _,
            window_id: _,
            which: _,
            mouse_btn,
            clicks: _,
            x,
            y,
        } => Ok(Some(Event::MouseDown(
            format!("{:?}", mouse_btn).into(),
            x,
            y,
        ))),
        SdlEvent::MouseButtonUp {
            timestamp: _,
            window_id: _,
            which: _,
            mouse_btn,
            clicks: _,
            x,
            y,
        } => Ok(Some(Event::MouseUp(
            format!("{:?}", mouse_btn).into(),
            x,
            y,
        ))),
        SdlEvent::MouseMotion {
            timestamp: _,
            window_id: _,
            which: _,
            x,
            y,
            mousestate: _,
            xrel,
            yrel,
        } => Ok(Some(Event::MouseMotion(x, y, xrel, yrel))),
        SdlEvent::MouseWheel {
            timestamp: _,
            window_id: _,
            which: _,
            x,
            y,
            direction: _,
        } => Ok(Some(Event::MouseWheel(x, y))),
        _ => Ok(None),
    }
}
