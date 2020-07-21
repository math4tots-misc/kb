use super::ast::*;
use super::lexer::*;
use super::BasicError;
use super::Binop;
use super::Mark;
use super::Unop;
use std::rc::Rc;

type Prec = i64;
const ADD_PREC: Prec = 60;
const MUL_PREC: Prec = 80;
const UNARY_PREC: Prec = 100;
const POSTFIX_PREC: Prec = 1000;

pub fn parse(source: &Rc<Source>) -> Result<File, BasicError> {
    let toks = lex(source)?;
    let mut parser = Parser {
        source: source.clone(),
        toks,
        i: 0,
    };
    let file = parser.file()?;
    Ok(file)
}

struct Parser<'a> {
    source: Rc<Source>,
    toks: Vec<(Token<'a>, Mark)>,
    i: usize,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> &Token<'a> {
        &self.toks[self.i].0
    }
    fn mark(&self) -> Mark {
        self.toks[self.i].1.clone()
    }
    fn at<P: Into<Pat<'a>>>(&self, p: P) -> bool {
        let p = p.into();
        p.matches(self.peek())
    }
    fn gettok(&mut self) -> Token<'a> {
        self.i += 1;
        std::mem::replace(&mut self.toks[self.i - 1].0, Token::EOF)
    }
    fn expect<'b, P: Into<Pat<'b>>>(&mut self, p: P) -> Result<Token<'a>, BasicError> {
        let p = p.into();
        if p.matches(self.peek()) {
            Ok(self.gettok())
        } else {
            Err(BasicError {
                marks: vec![self.mark()],
                message: format!("Expected {:?} but got {:?}", p, self.peek()),
            })
        }
    }
    fn expect_name(&mut self) -> Result<Rc<String>, BasicError> {
        Ok(self.expect(Pat::Name)?.name().unwrap().to_owned().into())
    }
    fn consume<P: Into<Pat<'a>>>(&mut self, p: P) -> bool {
        if self.at(p) {
            self.gettok();
            true
        } else {
            false
        }
    }
}

impl<'a> Parser<'a> {
    fn file(&mut self) -> Result<File, BasicError> {
        let mark = self.mark();
        let mut imports = Vec::new();
        let mut funcs = Vec::new();
        let mut stmts = Vec::new();
        self.consume_delim();
        while !self.at(Token::EOF) {
            match self.peek() {
                Token::Name("import") => imports.push(self.import_()?),
                Token::Name("def") => funcs.push(self.func()?),
                _ => stmts.push(self.stmt()?),
            }
            self.consume_delim();
        }
        Ok(File {
            source: self.source.clone(),
            imports,
            funcs,
            body: Stmt {
                mark,
                desc: StmtDesc::Block(stmts),
            },
            vars: vec![],
        })
    }
    fn import_(&mut self) -> Result<Import, BasicError> {
        let mark = self.mark();
        let mut module_name = self.expect_name()?;
        let mut last_part = module_name.clone();
        while self.consume(Token::Dot) {
            last_part = self.expect_name()?;
            module_name = format!("{}.{}", module_name, last_part).into();
        }
        if self.consume(Token::Name("as")) {
            last_part = self.expect_name()?;
        }
        Ok(Import {
            mark,
            module_name,
            alias: last_part,
            unique_name: "".to_owned().into(),
        })
    }
    fn func(&mut self) -> Result<FuncDisplay, BasicError> {
        let mark = self.mark();
        self.expect(Token::Name("def"))?;
        let name = self.expect_name()?;
        let params = {
            let mut params = Vec::new();
            self.expect(Token::LParen)?;
            while !self.consume(Token::RParen) {
                params.push(self.expect_name()?);
                if !self.consume(Token::Comma) {
                    self.expect(Token::RParen)?;
                    break;
                }
            }
            params
        };
        let body = self.block()?;
        Ok(FuncDisplay {
            mark,
            short_name: name,
            params,
            body,

            vars: vec![],
            as_var: None,
        })
    }
    fn block(&mut self) -> Result<Stmt, BasicError> {
        let mark = self.mark();
        let mut stmts = Vec::new();
        while !self.consume(Token::Name("end")) {
            let stmt = self.stmt()?;
            stmts.push(stmt);
        }
        Ok(Stmt {
            mark,
            desc: StmtDesc::Block(stmts),
        })
    }
    fn stmt(&mut self) -> Result<Stmt, BasicError> {
        let mark = self.mark();
        match self.peek() {
            Token::Name("print") => {
                self.gettok();
                let arg = self.expr(0)?;
                Ok(Stmt {
                    mark,
                    desc: StmtDesc::Print(arg.into()),
                })
            }
            _ => {
                let expr = self.expr(0)?;
                self.delim()?;
                Ok(Stmt {
                    mark,
                    desc: StmtDesc::Expr(expr),
                })
            }
        }
    }
    fn expr(&mut self, prec: Prec) -> Result<Expr, BasicError> {
        let mut e = self.atom()?;
        while precof(self.peek()) >= prec {
            e = self.infix(e)?;
        }
        Ok(e)
    }
    fn infix(&mut self, e: Expr) -> Result<Expr, BasicError> {
        let mark = self.mark();
        let token = self.gettok();
        match token {
            Token::Dot => {
                let attr = self.expect_name()?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::GetAttr(e.into(), attr),
                })
            }
            Token::LParen => {
                let mut args = Vec::new();
                while !self.consume(Token::RParen) {
                    args.push(self.expr(0)?);
                    if !self.consume(Token::Comma) {
                        self.expect(Token::RParen)?;
                        break;
                    }
                }
                Ok(Expr {
                    mark,
                    desc: ExprDesc::CallFunc(e.into(), args),
                })
            }
            Token::Plus
            | Token::Minus
            | Token::Star
            | Token::Slash
            | Token::Slash2
            | Token::Percent => {
                let op = match token {
                    Token::Plus => Binop::Add,
                    Token::Minus => Binop::Subtract,
                    Token::Star => Binop::Multiply,
                    Token::Slash => Binop::Divide,
                    Token::Slash2 => Binop::TruncDivide,
                    Token::Percent => Binop::Remainder,
                    _ => panic!("binop {:?}", token),
                };
                let prec = precof(&token);
                let rhs = self.expr(prec + 1)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Binop(op, e.into(), rhs.into()),
                })
            }
            _ => Err(BasicError {
                marks: vec![mark],
                message: format!("Expected infix operator"),
            }),
        }
    }
    fn atom(&mut self) -> Result<Expr, BasicError> {
        let mark = self.mark();
        match self.peek() {
            Token::LParen => {
                self.gettok();
                let expr = self.expr(0)?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Token::Number(x) => {
                let x = *x;
                self.gettok();
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Number(x),
                })
            }
            Token::String(_) => {
                let s = self.gettok().string().unwrap();
                Ok(Expr {
                    mark,
                    desc: ExprDesc::String(s.into()),
                })
            }
            Token::RawString(s) => {
                let s = Rc::new((*s).to_owned());
                Ok(Expr {
                    mark,
                    desc: ExprDesc::String(s),
                })
            }
            Token::Minus | Token::Plus => {
                let tok = self.gettok();
                let op = match tok {
                    Token::Minus => Unop::Negative,
                    Token::Plus => Unop::Positive,
                    _ => panic!("parse unop: {:?}", tok),
                };
                let expr = self.expr(UNARY_PREC)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Unop(op, expr.into()),
                })
            }
            Token::Name("nil") => {
                self.gettok();
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Nil,
                })
            }
            Token::Name(_) => {
                let name = self.expect_name()?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::GetVar(name),
                })
            }
            _ => Err(BasicError {
                marks: vec![mark],
                message: format!("Expected expression but got {:?}", self.peek()),
            }),
        }
    }
    fn consume_delim(&mut self) {
        while self.at(Token::Newline) || self.at(Token::Semicolon) {
            self.gettok();
        }
    }
    fn delim(&mut self) -> Result<(), BasicError> {
        match self.peek() {
            Token::RBrace | Token::EOF | Token::Semicolon | Token::Newline => (),
            t => {
                return Err(BasicError {
                    marks: vec![self.mark()],
                    message: format!("Expected delimiter but got {:?}", t),
                })
            }
        }
        self.consume_delim();
        Ok(())
    }
}

fn precof<'a>(tok: &Token<'a>) -> Prec {
    match tok {
        Token::Minus | Token::Plus => ADD_PREC,
        Token::Star | Token::Slash | Token::Slash2 | Token::Percent => MUL_PREC,
        Token::LParen => POSTFIX_PREC,
        _ => -1,
    }
}

#[derive(Debug, Clone)]
enum Pat<'a> {
    Exact(Token<'a>),
    Keyword(&'a str),
    Name,
}

impl<'a> Pat<'a> {
    fn matches<'b>(&self, tok: &Token<'b>) -> bool {
        match self {
            Pat::Exact(t) => t == tok,
            Pat::Keyword(t) => match tok {
                Token::Name(name) => t == name,
                _ => false,
            },
            Pat::Name => match tok {
                Token::Name(_) => true,
                _ => false,
            },
        }
    }
}

impl<'a> From<Token<'a>> for Pat<'a> {
    fn from(t: Token<'a>) -> Pat<'a> {
        Pat::Exact(t)
    }
}

impl<'a> From<&'a str> for Pat<'a> {
    fn from(s: &'a str) -> Pat<'a> {
        Pat::Keyword(s)
    }
}
