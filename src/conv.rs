use crate::rterr;
use crate::Val;
use a2d::Color;

impl Val {
    pub fn to_color(&self) -> Result<Color, Val> {
        match self {
            Val::List(list) if list.borrow().len() == 4 || list.borrow().len() == 3 => {
                let list = list.borrow();
                let r = list[0].expect_number()? as f32;
                let g = list[1].expect_number()? as f32;
                let b = list[2].expect_number()? as f32;
                let a = if list.len() == 4 {
                    list[3].expect_number()? as f32
                } else {
                    1.0
                };
                Ok([r, g, b, a].into())
            }
            _ => Err(rterr(format!(
                "Expected color, but got a {:?}",
                self.type_()
            ))),
        }
    }

    pub fn to_number_tuple(&self, len: usize) -> Result<Vec<f64>, Val> {
        match self {
            Val::List(list) if list.borrow().len() == len => {
                let mut ret = Vec::new();
                for x in list.borrow().iter() {
                    ret.push(x.expect_number()?);
                }
                Ok(ret)
            }
            _ => Err(rterr(format!(
                "Expected list of {} numbers, but got {:?}",
                len,
                self,
            ))),
        }
    }

    pub fn to_number_pair(&self) -> Result<(f64, f64), Val> {
        let pair = self.to_number_tuple(2)?;
        Ok((pair[0], pair[1]))
    }
}

impl From<Event> for Val {
    fn from(event: Event) -> Self {
        match event {
            Event::Quit => vec!["Quit".into()].into(),
            Event::Text(string) => vec!["Text".into(), string.into()].into(),
            Event::Key(string) => vec!["Key".into(), string.into()].into(),
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

#[derive(Debug, Clone)]
pub enum Event {
    Quit,
    Text(String),
    Key(String),
    MouseDown(String, f64, f64),
    MouseUp(String, f64, f64),
    MouseMotion(i32, i32, i32, i32),
    MouseWheel(i32, i32),
}
