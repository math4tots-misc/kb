use super::BasicError;
use super::Mark;
use super::RcStr;
use super::Source;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Name(&'a str),
    Number(f64),
    RawString(&'a str),
    String(RcStr),
    EOF,

    // Single character symbols
    Newline,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Dollar,
    Dot,
    Dot2,
    Colon,
    Comma,
    Semicolon,
    Percent,
    Plus,
    Minus,
    Star,
    Slash,
    Slash2,
    Eq,
    Bar,
    Excalamation,

    Eq2,
    Ne,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

impl<'a> Token<'a> {
    pub fn name_or_keyword(&self) -> Option<&str> {
        if let Token::Name(s) = self {
            Some(s)
        } else {
            None
        }
    }
    #[allow(dead_code)]
    pub fn number(&self) -> Option<f64> {
        if let Token::Number(x) = self {
            Some(*x)
        } else {
            None
        }
    }
    #[allow(dead_code)]
    pub fn raw_string(&self) -> Option<&str> {
        if let Token::RawString(x) = self {
            Some(x)
        } else {
            None
        }
    }
    #[allow(dead_code)]
    pub fn string(self) -> Option<RcStr> {
        if let Token::String(x) = self {
            Some(x)
        } else {
            None
        }
    }
}

pub fn lex(source: &Rc<Source>) -> Result<Vec<(Token, Mark)>, BasicError> {
    let s = &source.data;
    let mut ret = Vec::<(Token, Mark)>::new();
    let mut state = State::Neutral;
    let mut last_ig_ws = 0;
    let mut pstack = ParenStack::new();
    let mut chars = Chars::new(s);
    while let Some(c) = chars.next() {
        let i = chars.index - c.len_utf8();
        let mark = Mark {
            source: source.clone(),
            pos: i,
        };
        match state {
            State::Neutral => {
                if c.is_whitespace() && (c != '\n' || pstack.ignore_newline()) {
                    // skip whitespace
                    // We also keep track of the last ignored whitespace
                    // to figure out when tokens should be combined
                    last_ig_ws = i;
                    state = State::Neutral;
                } else if c.is_ascii_digit() {
                    state = State::Digits(i);
                } else if c == '_' || c.is_alphanumeric() {
                    state = State::Name(i);
                } else if c == '"' || c == '\'' {
                    if let Some((Token::Name("r"), _)) = ret.last() {
                        ret.pop().unwrap();
                        state = State::RawString(c, i + c.len_utf8());
                    } else {
                        state = State::String(c, String::new());
                    }
                } else {
                    let tok = match c {
                        '\0' => Some(Token::EOF),
                        '\n' => Some(Token::Newline),
                        '(' => Some(Token::LParen),
                        ')' => Some(Token::RParen),
                        '[' => Some(Token::LBracket),
                        ']' => Some(Token::RBracket),
                        '{' => Some(Token::LBrace),
                        '}' => Some(Token::RBrace),
                        '$' => Some(Token::Dollar),
                        '.' => Some(
                            if ret.last().map(|p| &p.0) == Some(&Token::Dot) && last_ig_ws < i - 1 {
                                ret.pop().unwrap();
                                Token::Dot2
                            } else {
                                Token::Dot
                            },
                        ),
                        ':' => Some(Token::Colon),
                        ',' => Some(Token::Comma),
                        ';' => Some(Token::Semicolon),
                        '+' => Some(Token::Plus),
                        '-' => Some(Token::Minus),
                        '*' => Some(Token::Star),
                        '/' => Some(
                            if ret.last().map(|p| &p.0) == Some(&Token::Slash) && last_ig_ws < i - 1
                            {
                                ret.pop().unwrap();
                                Token::Slash2
                            } else {
                                Token::Slash
                            },
                        ),
                        '%' => Some(Token::Percent),
                        '|' => Some(Token::Bar),
                        '!' => Some(Token::Excalamation),
                        '<' => Some(Token::LessThan),
                        '>' => Some(Token::GreaterThan),
                        '=' => Some({
                            if last_ig_ws < i - 1 {
                                match ret.last() {
                                    Some((Token::LessThan, _)) => {
                                        ret.pop().unwrap();
                                        Token::LessThanOrEqual
                                    }
                                    Some((Token::GreaterThan, _)) => {
                                        ret.pop().unwrap();
                                        Token::GreaterThanOrEqual
                                    }
                                    Some((Token::Eq, _)) => {
                                        ret.pop().unwrap();
                                        Token::Eq2
                                    }
                                    Some((Token::Excalamation, _)) => {
                                        ret.pop().unwrap();
                                        Token::Ne
                                    }
                                    _ => Token::Eq,
                                }
                            } else {
                                Token::Eq
                            }
                        }),
                        _ => None,
                    };
                    if let Some(tok) = tok {
                        match tok {
                            Token::LParen | Token::LBracket => pstack.push(true),
                            Token::LBrace => pstack.push(false),
                            Token::RParen | Token::RBracket | Token::RBrace => match pstack.pop() {
                                Ok(()) => {}
                                Err(message) => {
                                    return Err(BasicError {
                                        marks: vec![mark],
                                        message,
                                        help: None,
                                    })
                                }
                            },
                            _ => (),
                        }
                        ret.push((tok, mark));
                        state = State::Neutral;
                    } else {
                        return Err(BasicError {
                            marks: vec![mark],
                            message: format!("Unrecognized token: {}", c),
                            help: None,
                        });
                    }
                }
            }
            State::Digits(start) => {
                if c.is_ascii_digit() {
                    state = State::Digits(start);
                } else if c == '.' {
                    state = State::Number(start);
                } else {
                    chars.put_back(c);
                    state = State::Number(start);
                }
            }
            State::Number(start) => {
                if c.is_ascii_digit() {
                    state = State::Number(start);
                } else {
                    let n: f64 = s[start..i].parse().unwrap();
                    ret.push((
                        Token::Number(n),
                        Mark {
                            source: source.clone(),
                            pos: start,
                        },
                    ));
                    chars.put_back(c);
                    state = State::Neutral;
                }
            }
            State::Name(start) => {
                if c == '_' || c.is_alphanumeric() {
                    state = State::Name(start);
                } else {
                    ret.push((
                        Token::Name(&s[start..i]),
                        Mark {
                            source: source.clone(),
                            pos: start,
                        },
                    ));
                    chars.put_back(c);
                    state = State::Neutral;
                }
            }
            State::RawString(q, start) => {
                if c == q {
                    ret.push((
                        Token::RawString(&s[start..i]),
                        Mark {
                            source: source.clone(),
                            pos: start,
                        },
                    ));
                    state = State::Neutral;
                } else {
                    state = State::RawString(q, start);
                }
            }
            State::String(q, mut string) => {
                if c == q {
                    ret.push((
                        Token::String(string.into()),
                        Mark {
                            source: source.clone(),
                            pos: i,
                        },
                    ));
                    state = State::Neutral;
                } else if c == '\\' {
                    state = State::StringEscaped(q, string);
                } else {
                    string.push(c);
                    state = State::String(q, string);
                }
            }
            State::StringEscaped(q, mut string) => {
                let s = match c {
                    '\\' => "\\",
                    '\'' => "\'",
                    '\"' => "\"",
                    't' => "\t",
                    'n' => "\n",
                    'r' => "\r",
                    _ => {
                        return Err(BasicError {
                            marks: vec![Mark {
                                source: source.clone(),
                                pos: i,
                            }],
                            message: format!("Invalid string escape ({})", c),
                            help: None,
                        })
                    }
                };
                string.push_str(s);
                state = State::String(q, string);
            }
        }
    }
    if let State::Neutral = &state {
        Ok(ret)
    } else {
        Err(BasicError {
            marks: vec![Mark {
                source: source.clone(),
                pos: s.len(),
            }],
            message: format!("Expected more input: {:?}", state),
            help: None,
        })
    }
}

#[derive(Debug)]
enum State {
    Neutral,
    Digits(usize),
    Number(usize),
    Name(usize),
    RawString(char, usize),
    String(char, String),
    StringEscaped(char, String),
}

struct ParenStack {
    stack: Vec<bool>,
}

impl ParenStack {
    pub fn new() -> ParenStack {
        ParenStack { stack: Vec::new() }
    }
    pub fn push(&mut self, ignore_newline: bool) {
        self.stack.push(ignore_newline)
    }
    pub fn pop(&mut self) -> Result<(), String> {
        match self.stack.pop() {
            Some(_) => Ok(()),
            None => Err(format!("Mismatched grouping symbols")),
        }
    }
    pub fn ignore_newline(&self) -> bool {
        self.stack.last().cloned().unwrap_or(false)
    }
}

struct Chars<'a> {
    index: usize,
    peek: Option<char>,
    chars: std::iter::Chain<std::str::Chars<'a>, std::vec::IntoIter<char>>,
}

impl<'a> Chars<'a> {
    fn new(s: &str) -> Chars {
        Chars {
            index: 0,
            peek: None,
            chars: s.chars().chain(vec!['\0']),
        }
    }
    fn next(&mut self) -> Option<char> {
        let ch = if let Some(ch) = std::mem::replace(&mut self.peek, None) {
            Some(ch)
        } else {
            self.chars.next()
        };
        if let Some(ch) = ch {
            self.index += ch.len_utf8();
        }
        ch
    }
    fn put_back(&mut self, c: char) {
        assert!(self.peek.is_none());
        self.peek = Some(c);
        self.index -= c.len_utf8();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    fn mksrc(data: &str) -> Rc<Source> {
        Source {
            name: "[for-test]".into(),
            data: data.into(),
        }
        .into()
    }

    fn lex(src: &Rc<Source>) -> Vec<Token> {
        super::lex(src).unwrap().into_iter().map(|p| p.0).collect()
    }

    #[test]
    fn raw_string_literals() {
        let src = mksrc(r####" r"hi" "####);
        assert_eq!(lex(&src), vec![RawString("hi"), EOF]);
    }

    #[test]
    fn misc() {
        let src = mksrc(r##" x = r"hi" "##);
        assert_eq!(lex(&src), vec![Name("x"), Eq, RawString("hi"), EOF]);
    }
}
