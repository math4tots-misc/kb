use crate::rterr;
use crate::Val;

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
            _ => Err(rterr(format!(
                "Expected color, but got a {:?}",
                self.type_()
            ))),
        }
    }
}

impl From<Event> for Val {
    fn from(event: Event) -> Self {
        match event {
            Event::Quit => vec!["Quit".into()].into(),
            Event::Text(string) => vec!["Text".into(), string.into()].into(),
            Event::Key(string) => vec!["Key".into(), string.into()].into(),
            Event::KeyRepeat(string) => vec!["KeyRepeat".into(), string.into()].into(),
            Event::MouseDown(string, x, y) => vec![
                "MouseDown".into(),
                string.into(),
                (x as f64).into(),
                (y as f64).into(),
            ]
            .into(),
            Event::MouseUp(string, x, y) => vec![
                "MouseUp".into(),
                string.into(),
                (x as f64).into(),
                (y as f64).into(),
            ]
            .into(),
            Event::MouseMotion(x, y, xrel, yrel) => vec![
                "MouseMotion".into(),
                (x as f64).into(),
                (y as f64).into(),
                (xrel as f64).into(),
                (yrel as f64).into(),
            ]
            .into(),
            Event::MouseWheel(x, y) => {
                vec!["MouseWheel".into(), (x as f64).into(), (y as f64).into()].into()
            }
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
        Self { r, g, b, a }
    }
}

#[derive(Debug, Clone)]
pub enum Event {
    Quit,
    Text(String),
    Key(String),
    KeyRepeat(String),
    MouseDown(String, i32, i32),
    MouseUp(String, i32, i32),
    MouseMotion(i32, i32, i32, i32),
    MouseWheel(i32, i32),
}
