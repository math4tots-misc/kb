use crate::Val;
use crate::rterr;

impl Val {
    pub fn to_color(&self) -> Result<Color, Val> {
        match self {
            Val::List(list) if list.borrow().len() == 4 || list.borrow().len() == 3 => {
                let list = list.borrow();
                let r = list[0].expect_number()?;
                let g = list[1].expect_number()?;
                let b = list[2].expect_number()?;
                let a = if list.len() == 4 {
                    list[3].expect_number()?
                } else {
                    1.0
                };
                Ok(Color::rgba(r, g, b, a))
            }
            _ => Err(rterr(format!("Expected color, but got a {:?}", self.type_()))),
        }
    }
}

impl From<Event> for Val {
    fn from(event: Event) -> Self {
        match event {
            Event::Quit => vec![
                "Quit".into(),
            ].into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Color {
    pub r: f64,
    pub g: f64,
    pub b: f64,
    pub a: f64,
}

impl Color {
    pub fn rgba(r: f64, g: f64, b: f64, a: f64) -> Self {
        Self {
            r, g, b, a
        }
    }
}

#[derive(Debug, Clone)]
pub enum Event {
    Quit,
}
