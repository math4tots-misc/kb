use super::ast::*;
use super::lexer::*;
use super::ArgSpec;
use super::ArithmeticBinop;
use super::ArithmeticUnop;
use super::BasicError;
use super::Binop;
use super::Mark;
use super::RcStr;
use super::Tenop;
use super::Unop;
use super::Val;
use super::Zop;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

type Prec = i64;
const OR_PREC: Prec = 70;
const AND_PREC: Prec = 80;
const NOT_PREC: Prec = 90;
const CMP_PREC: Prec = 100;
const ADD_PREC: Prec = 120;
const MUL_PREC: Prec = 160;
const UNARY_PREC: Prec = 200;
const POW_PREC: Prec = 300;
const POSTFIX_PREC: Prec = 1000;

const CONTROL_KEYWORDS: &[&str] = &[
    // mixed (used in both global and statement level)
    "end",
    // ============================= for global items =============================
    "test", "fn", "import", "as", "class",
    // ============================= statement level =============================
    "var", "if", "elif", "else", "then", "while", "for", "to", "try", "catch", "throw", "assert",
    "return", "print",
    // ============================= legacy all-caps keywords =============================
    "RETURN", "PRINT", "GOTO", "DIM", "LET", "IF", "ELSEIF", "ELSE", "THEN", "DO", "WHILE", "FOR",
    "LOOP", "TO", "FUNCTION", "END", "SUB", "TEST", "SELECT", "CASE",
];

const RESERVED_KEYWORDS: &[&str] = &["switch"];

const EXPR_KEYWORDS: &[&str] = &[
    "and", "or", "in", "is", "not", "yield",
    // ============================= legacy all-caps keywords =============================
    "AND", "OR", "IN", "NOT",
];

/// keywords for function-like builtin operators
/// These 'pseudo-functions' are all-caps
const OP_KEYWORDS: &[&str] = &["NEXT", "DISASM", "CAT", "DELETE", "SORTED", "SEND"];

const LITERAL_KEYWORDS: &[&str] = &["true", "false", "nil"];

/// Zero argument operators
const ZOPS: &[(&'static str, Zop)] = &[("TIME", Zop::Time)];

const UNOPS: &[(&'static str, Unop)] = &[
    ("SLEEP", Unop::Sleep),
    ("CLASS", Unop::GetClass),
    ("NEW", Unop::New),
    ("NAME", Unop::Name),
    ("STR", Unop::Str),
    ("REPR", Unop::Repr),
    ("BYTES", Unop::Bytes),
    ("LIST", Unop::List),
    ("SET", Unop::Set),
    ("MAP", Unop::Map),
    ("LEN", Unop::Len),
    ("TYPE", Unop::Type),
    ("POP", Unop::Pop),
    ("SORT", Unop::Sort),
    ("SIN", Unop::Arithmetic(ArithmeticUnop::Sin)),
    ("COS", Unop::Arithmetic(ArithmeticUnop::Cos)),
    ("TAN", Unop::Arithmetic(ArithmeticUnop::Tan)),
    ("ASIN", Unop::Arithmetic(ArithmeticUnop::ASin)),
    ("ACOS", Unop::Arithmetic(ArithmeticUnop::ACos)),
    ("ATAN", Unop::Arithmetic(ArithmeticUnop::ATan)),
];

const BINOPS: &[(&'static str, Binop)] = &[
    ("ADD", Binop::Add),
    ("EXTEND", Binop::Extend),
    ("REMOVE", Binop::Remove),
    ("ATAN2", Binop::Arithmetic(ArithmeticBinop::ATan2)),
    ("METHOD", Binop::Method),
];

const TENOPS: &[(&'static str, Tenop)] = &[("SLICE", Tenop::Slice)];

pub fn parse(source: &Rc<Source>) -> Result<File, BasicError> {
    let toks = lex(source)?;
    let keywords: HashSet<&'static str> = CONTROL_KEYWORDS
        .iter()
        .chain(EXPR_KEYWORDS)
        .chain(OP_KEYWORDS)
        .chain(ZOPS.iter().map(|(name, _)| name))
        .chain(UNOPS.iter().map(|(name, _)| name))
        .chain(BINOPS.iter().map(|(name, _)| name))
        .chain(TENOPS.iter().map(|(name, _)| name))
        .chain(LITERAL_KEYWORDS)
        .chain(RESERVED_KEYWORDS)
        .map(|s| *s)
        .collect();
    let zop_map: HashMap<&'static str, Zop> = ZOPS.to_vec().into_iter().collect();
    let unop_map: HashMap<&'static str, Unop> = UNOPS.to_vec().into_iter().collect();
    let binop_map: HashMap<&'static str, Binop> = BINOPS.to_vec().into_iter().collect();
    let tenop_map: HashMap<&'static str, Tenop> = TENOPS.to_vec().into_iter().collect();
    let mut parser = Parser {
        source: source.clone(),
        toks,
        i: 0,
        keywords,
        zop_map,
        unop_map,
        binop_map,
        tenop_map,
    };
    let file = parser.file()?;
    Ok(file)
}

struct Parser<'a> {
    source: Rc<Source>,
    toks: Vec<(Token<'a>, Mark)>,
    i: usize,
    keywords: HashSet<&'static str>,
    zop_map: HashMap<&'static str, Zop>,
    unop_map: HashMap<&'static str, Unop>,
    binop_map: HashMap<&'static str, Binop>,
    tenop_map: HashMap<&'static str, Tenop>,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> &Token<'a> {
        &self.toks[self.i].0
    }
    fn lookahead(&self, n: usize) -> Option<&Token<'a>> {
        self.toks.get(self.i + n).map(|pair| &pair.0)
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
        self.toks[self.i - 1].0.clone()
    }
    fn expect<'b, P: Into<Pat<'b>>>(&mut self, p: P) -> Result<Token<'a>, BasicError> {
        let p = p.into();
        if p.matches(self.peek()) {
            Ok(self.gettok())
        } else {
            Err(BasicError {
                marks: vec![self.mark()],
                message: format!("Expected {:?} but got {:?}", p, self.peek()),
                help: None,
            })
        }
    }
    fn expect_name(&mut self) -> Result<RcStr, BasicError> {
        let mark = self.mark();
        let token = self.expect(Pat::Name)?;
        let name = token.name_or_keyword().unwrap();
        if self.keywords.contains(&name) {
            Err(BasicError {
                marks: vec![mark],
                message: format!("Expected name but got keyword"),
                help: None,
            })
        } else {
            Ok(name.to_owned().into())
        }
    }
    fn expect_label(&mut self) -> Result<RcStr, BasicError> {
        match self.peek() {
            Token::Number(x) => {
                let x = *x;
                self.gettok();
                Ok(format!("{}", x).into())
            }
            Token::Name(_) => {
                let name = self.expect_name()?;
                Ok(name)
            }
            _ => Err(BasicError::new(
                vec![self.mark()],
                format!("Expected label but got {:?}", self.peek()),
            )),
        }
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
        let mut clss = Vec::new();
        let mut stmts = Vec::new();
        self.consume_delim();
        while !self.at(Token::EOF) {
            match self.peek() {
                Token::Name("import") => imports.push(self.import_()?),
                Token::Name("fn")
                | Token::Name("test")
                | Token::Name("FUNCTION")
                | Token::Name("SUB") => funcs.push(self.func()?),
                Token::Name("class") => {
                    self.cls(&mut clss, &mut funcs)?;
                }
                _ => stmts.extend(self.maybe_labeled_stmt()?),
            }
            self.delim()?;
        }
        Ok(File {
            source: self.source.clone(),
            imports,
            funcs,
            clss,
            body: Stmt {
                mark,
                desc: StmtDesc::Block(stmts),
            },
            vars: vec![],
        })
    }
    fn import_(&mut self) -> Result<Import, BasicError> {
        let mark = self.mark();
        self.expect(Token::Name("import"))?;
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
    fn at_end(&mut self) -> bool {
        self.at(Token::Name("end")) || self.at(Token::Name("END"))
    }
    fn at_elif(&mut self) -> bool {
        self.at(Token::Name("elif")) || self.at(Token::Name("ELSEIF"))
    }
    fn at_else(&mut self) -> bool {
        self.at(Token::Name("else")) || self.at(Token::Name("ELSE"))
    }
    fn expect_end(&mut self) -> Result<(), BasicError> {
        if !self.consume(Token::Name("END")) {
            self.expect(Token::Name("end"))?;
        }
        Ok(())
    }
    fn consume_elif(&mut self) -> bool {
        self.consume(Token::Name("elif")) || self.consume(Token::Name("ELSEIF"))
    }
    fn consume_else(&mut self) -> bool {
        self.consume(Token::Name("else")) || self.consume(Token::Name("ELSE"))
    }
    fn func(&mut self) -> Result<FuncDisplay, BasicError> {
        let mark = self.mark();

        // the leading keyword
        //   * FUNCTION/SUB/fn are all equivalent
        //   * test is almost equivalent, except that it implies [test]
        let test = if self.consume(Token::Name("test")) {
            true
        } else {
            if !self.consume(Token::Name("FUNCTION")) && !self.consume(Token::Name("SUB")) {
                self.expect(Token::Name("fn"))?;
            }
            false
        };

        // A '*' after the leading function keyword implies this a generator function
        let generator = self.consume(Token::Star);

        let name = self.expect_name()?;
        let argspec = if self.consume(Token::LParen) {
            let mut req = Vec::new();
            let mut def = Vec::new();
            let mut var = None;
            'fin: loop {
                // required parameters
                while self.at(Pat::Name) && self.lookahead(1) != Some(&Token::Eq) {
                    req.push(self.expect_name()?);

                    // no comma => parameter spec is done
                    if !self.consume(Token::Comma) {
                        self.expect(Token::RParen)?;
                        break 'fin;
                    }
                }

                // default parameters
                while self.at(Pat::Name) {
                    let name = self.expect_name()?;
                    self.expect(Token::Eq)?;
                    let val = self.constexpr()?;
                    def.push((name, val));

                    // no comma => parameter spec is done
                    if !self.consume(Token::Comma) {
                        self.expect(Token::RParen)?;
                        break 'fin;
                    }
                }

                // variadic parameter
                if self.consume(Token::Star) {
                    var = Some(self.expect_name()?);
                }

                self.consume(Token::Comma);
                self.expect(Token::RParen)?;
                break;
            }
            ArgSpec { req, def, var }
        } else {
            ArgSpec::empty()
        };
        let body = if self.consume(Token::Eq) {
            Stmt {
                mark: mark.clone(),
                desc: StmtDesc::Return(Some(self.expr(0)?)),
            }
        } else {
            self.block()?
        };
        Ok(FuncDisplay {
            mark,
            generator,
            test,
            short_name: name,
            argspec,
            body,
            vars: vec![],
            as_var: None,
        })
    }
    fn cls(
        &mut self,
        clss: &mut Vec<ClassDisplay>,
        funcs: &mut Vec<FuncDisplay>,
    ) -> Result<(), BasicError> {
        let mark = self.mark();
        self.expect(Token::Name("class"))?;
        let class_short_name = self.expect_name()?;
        let mut bases = Vec::new();
        if self.consume(Token::LParen) {
            bases = self.args()?;
        }
        self.delim()?;
        let mut method_pairs = Vec::new();
        while !self.at_end() {
            let mut func = self.func()?;
            self.delim()?;
            let func_short_name = func.short_name;
            let method_short_name = format!("{}->{}", class_short_name, func_short_name);
            func.short_name = method_short_name.into();
            method_pairs.push(func_short_name);
            funcs.push(func);
        }
        self.expect_end()?;
        clss.push(ClassDisplay {
            mark,
            short_name: class_short_name,
            bases,
            methods: method_pairs,
            as_var: None,
        });
        Ok(())
    }
    fn block(&mut self) -> Result<Stmt, BasicError> {
        let block = self.block_body()?;
        self.expect_end()?;
        Ok(block)
    }
    fn block_body(&mut self) -> Result<Stmt, BasicError> {
        let mark = self.mark();
        let mut stmts = Vec::new();
        self.consume_delim();
        while !self.at_end() && !self.at_elif() && !self.at_else() && !self.at(Token::Name("catch"))
        {
            let new_stmts = self.maybe_labeled_stmt()?;
            self.delim()?;
            stmts.extend(new_stmts);
        }
        Ok(Stmt {
            mark,
            desc: StmtDesc::Block(stmts),
        })
    }
    fn maybe_labeled_stmt(&mut self) -> Result<Vec<Stmt>, BasicError> {
        let mut ret = Vec::new();

        // check for
        //   <label-name> :
        // style labels
        match self.peek() {
            Token::Name(name)
                if !self.keywords.contains(name) && self.lookahead(1) == Some(&Token::Colon) =>
            {
                let mark = self.mark();
                let label = self.expect_label()?;
                self.expect(Token::Colon)?;
                self.consume_delim();
                ret.push(Stmt {
                    mark,
                    desc: StmtDesc::Label(label),
                })
            }
            _ => {}
        }

        // check for line number labels
        // we check to see if a Number is immediately followed
        // by a Name (keyword or otherwise) or open parentheses
        // if this is not the case, we assume there is no line number
        if self.peek().number().is_some()
            && self
                .lookahead(1)
                .map(|t| t.name_or_keyword().is_some() || t == &Token::LParen)
                .unwrap_or(false)
        {
            let mark = self.mark();
            let label = self.expect_label()?;
            ret.push(Stmt {
                mark,
                desc: StmtDesc::Label(label),
            });
        }

        ret.push(self.stmt()?);

        Ok(ret)
    }
    fn stmt(&mut self) -> Result<Stmt, BasicError> {
        let mark = self.mark();
        match self.peek() {
            Token::Name("PRINT") | Token::Name("print") => {
                self.gettok();
                let arg = self.expr(0)?;
                Ok(Stmt {
                    mark,
                    desc: StmtDesc::Print(arg.into()),
                })
            }
            Token::Name("GOTO") => {
                self.gettok();
                let label = self.expect_label()?;
                Ok(Stmt {
                    mark,
                    desc: StmtDesc::Goto(label),
                })
            }
            Token::Name("RETURN") | Token::Name("return") => {
                self.gettok();
                let expr = if self.at_delim() {
                    None
                } else {
                    Some(self.expr(0)?)
                };
                Ok(Stmt {
                    mark,
                    desc: StmtDesc::Return(expr),
                })
            }
            Token::Name("DIM") | Token::Name("LET") => {
                self.gettok();
                let lhs = self.expr(0)?;
                self.expect(Token::Eq)?;
                self.rhs_assignment(mark, lhs)
            }
            Token::Name("if") | Token::Name("IF") => {
                self.gettok();
                let mut pairs = Vec::new();
                let mut other = None;
                loop {
                    let cond = self.expr(0)?;
                    self.delim()?;
                    let body = self.block_body()?;
                    pairs.push((cond, body));
                    if !self.consume_elif() {
                        if self.consume_else() {
                            self.delim()?;
                            other = Some(self.block_body()?.into());
                        }
                        self.expect_end()?;
                        break;
                    }
                }
                Ok(Stmt {
                    mark,
                    desc: StmtDesc::If(pairs, other),
                })
            }
            Token::Name("while") | Token::Name("WHILE") => {
                self.gettok();
                let cond = self.expr(0)?;
                self.delim()?;
                let body = self.block()?;
                Ok(Stmt {
                    mark,
                    desc: StmtDesc::While(cond, body.into()),
                })
            }
            Token::Name("for") => {
                self.gettok();
                let target = expr_to_assign_target(self.expr(UNARY_PREC)?)?;
                if self.consume(Token::Name("in")) {
                    let container = self.expr(0)?;
                    self.delim()?;
                    let body = self.block()?;
                    Ok(Stmt {
                        mark,
                        desc: StmtDesc::ForIn(target, container, body.into()),
                    })
                } else {
                    self.expect(Token::Eq)?;
                    let start = self.expr(0)?;
                    let inclusive =
                        if self.consume(Token::Name("TO")) || self.consume(Token::Name("to")) {
                            true
                        } else {
                            self.expect(Token::Dot2)?;
                            false
                        };
                    let end = self.expr(0)?;
                    let step =
                        if self.consume(Token::Name("STEP")) || self.consume(Token::Name("step")) {
                            self.constexpr_number()?
                        } else {
                            1.0
                        };
                    self.delim()?;
                    let body = self.block()?;
                    Ok(Stmt {
                        mark,
                        desc: StmtDesc::ForClassic(
                            target,
                            start,
                            end,
                            inclusive,
                            step,
                            body.into(),
                        ),
                    })
                }
            }
            Token::Name("assert") => {
                self.gettok();
                if self.consume(Token::Name("throw")) {
                    let body = if self.at_delim() {
                        self.block()?
                    } else {
                        let expr = self.expr(0)?;
                        Stmt {
                            mark: mark.clone(),
                            desc: StmtDesc::Expr(expr),
                        }
                    };
                    Ok(Stmt {
                        mark,
                        desc: StmtDesc::AssertThrow(body.into()),
                    })
                } else {
                    let cond = self.expr(0)?;
                    Ok(Stmt {
                        mark,
                        desc: StmtDesc::Assert(cond),
                    })
                }
            }
            Token::Name("try") => {
                self.gettok();
                let body = self.block_body()?.into();
                self.expect(Token::Name("catch"))?;
                let target = expr_to_assign_target(self.expr(UNARY_PREC)?)?;
                self.delim()?;
                let onerr = self.block()?.into();
                Ok(Stmt {
                    mark,
                    desc: StmtDesc::Try(body, target, onerr),
                })
            }
            Token::Name("throw") => {
                self.gettok();
                let expr = self.expr(0)?;
                Ok(Stmt {
                    mark,
                    desc: StmtDesc::Throw(expr),
                })
            }
            Token::Name(name)
                if !self.keywords.contains(name) && self.lookahead(1) == Some(&Token::Colon) =>
            {
                let label = self.expect_label()?;
                self.expect(Token::Colon)?;
                Ok(Stmt {
                    mark,
                    desc: StmtDesc::Label(label),
                })
            }
            Token::Name(name)
                if !self.keywords.contains(name) && self.lookahead(1) == Some(&Token::LeftArrow) =>
            {
                let name = self.expect_name()?;
                self.expect(Token::LeftArrow)?;
                let expr = self.expr(0)?;
                Ok(Stmt {
                    mark,
                    desc: StmtDesc::AssignGlobal(name, expr),
                })
            }
            _ => {
                let expr = self.expr(0)?;
                if self.consume(Token::Eq) {
                    self.rhs_assignment(mark, expr)
                } else {
                    Ok(Stmt {
                        mark,
                        desc: StmtDesc::Expr(expr),
                    })
                }
            }
        }
    }
    fn rhs_assignment(&mut self, mark: Mark, lhs: Expr) -> Result<Stmt, BasicError> {
        let mut exprs = vec![lhs, self.expr(0)?];
        while self.consume(Token::Eq) {
            exprs.push(self.expr(0)?);
        }

        let target = expr_to_assign_target(exprs.remove(0))?;
        let rhs = exprs.pop().unwrap();
        let mut other_targets = Vec::new();
        for expr in exprs {
            other_targets.push(expr_to_assign_target(expr)?);
        }

        Ok(Stmt {
            mark,
            desc: StmtDesc::Assign(target, other_targets, rhs.into()),
        })
    }
    fn constexpr(&mut self) -> Result<Val, BasicError> {
        let expr = self.expr(0)?;
        let mark = expr.mark;
        match expr.desc {
            ExprDesc::Nil => Ok(Val::Nil),
            ExprDesc::Bool(b) => Ok(Val::Bool(b)),
            ExprDesc::Number(x) => Ok(Val::Number(x)),
            ExprDesc::String(x) => Ok(Val::String(x)),
            _ => Err(BasicError {
                marks: vec![mark],
                message: format!("Expected a constant expression here"),
                help: None,
            }),
        }
    }
    fn constexpr_number(&mut self) -> Result<f64, BasicError> {
        let mark = self.mark();
        match self.constexpr()? {
            Val::Number(x) => Ok(x),
            _ => Err(BasicError {
                marks: vec![mark],
                message: format!("Expected a constant number here"),
                help: None,
            }),
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
            Token::RightArrow => {
                let method_name = self.expect_name()?;
                self.expect(Token::LParen)?;
                let (args, kwargs) = self.args_with_kwargs()?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::CallMethod(e.into(), method_name, args, kwargs),
                })
            }
            Token::LParen => {
                let (args, kwargs) = self.args_with_kwargs()?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::CallFunc(e.into(), args, kwargs),
                })
            }
            Token::LBracket => {
                if self.consume(Token::Colon) {
                    let upper = self.expr(0)?;
                    self.expect(Token::RBracket)?;
                    Ok(Expr {
                        mark: mark.clone(),
                        desc: ExprDesc::Tenop(
                            Tenop::Slice,
                            e.into(),
                            Expr {
                                mark: mark.clone(),
                                desc: ExprDesc::Nil,
                            }
                            .into(),
                            upper.into(),
                        ),
                    })
                } else {
                    let index = self.expr(0)?;
                    if self.consume(Token::Colon) {
                        if self.consume(Token::RBracket) {
                            Ok(Expr {
                                mark: mark.clone(),
                                desc: ExprDesc::Tenop(
                                    Tenop::Slice,
                                    e.into(),
                                    index.into(),
                                    Expr {
                                        mark: mark.clone(),
                                        desc: ExprDesc::Nil,
                                    }
                                    .into(),
                                ),
                            })
                        } else {
                            let upper = self.expr(0)?;
                            self.expect(Token::RBracket)?;
                            Ok(Expr {
                                mark,
                                desc: ExprDesc::Tenop(
                                    Tenop::Slice,
                                    e.into(),
                                    index.into(),
                                    upper.into(),
                                ),
                            })
                        }
                    } else {
                        self.expect(Token::RBracket)?;
                        Ok(Expr {
                            mark,
                            desc: ExprDesc::Binop(Binop::GetItem, e.into(), index.into()),
                        })
                    }
                }
            }
            Token::Plus
            | Token::Minus
            | Token::Star
            | Token::Slash
            | Token::Slash2
            | Token::Percent
            | Token::Caret
            | Token::Eq2
            | Token::Ne
            | Token::LessThan
            | Token::LessThanOrEqual
            | Token::GreaterThan
            | Token::GreaterThanOrEqual
            | Token::Name("in")
            | Token::Name("is")
            | Token::Name("not") => {
                let op = match token {
                    Token::Plus => Binop::Arithmetic(ArithmeticBinop::Add),
                    Token::Minus => Binop::Arithmetic(ArithmeticBinop::Subtract),
                    Token::Star => Binop::Arithmetic(ArithmeticBinop::Multiply),
                    Token::Slash => Binop::Arithmetic(ArithmeticBinop::Divide),
                    Token::Slash2 => Binop::Arithmetic(ArithmeticBinop::TruncDivide),
                    Token::Percent => Binop::Arithmetic(ArithmeticBinop::Remainder),
                    Token::Caret => Binop::Arithmetic(ArithmeticBinop::Exponentiate),
                    Token::Eq2 => Binop::Equal,
                    Token::Ne => Binop::NotEqual,
                    Token::LessThan => Binop::LessThan,
                    Token::LessThanOrEqual => Binop::LessThanOrEqual,
                    Token::GreaterThan => Binop::GreaterThan,
                    Token::GreaterThanOrEqual => Binop::GreaterThanOrEqual,
                    Token::Name("in") => Binop::In,
                    Token::Name("is") => {
                        if self.consume(Token::Name("not")) {
                            Binop::IsNot
                        } else {
                            Binop::Is
                        }
                    }
                    Token::Name("not") => {
                        self.expect(Token::Name("in"))?;
                        Binop::NotIn
                    }
                    _ => panic!("binop {:?}", token),
                };
                let prec = precof(&token);
                let rhs = self.expr(match token {
                    // right associative
                    Token::Caret => prec,
                    // left associative (most things besides '^')
                    _ => prec + 1,
                })?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Binop(op, e.into(), rhs.into()),
                })
            }
            Token::Name("AND") | Token::Name("and") => {
                let rhs = self.expr(AND_PREC + 1)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::And(e.into(), rhs.into()),
                })
            }
            Token::Name("OR") | Token::Name("or") => {
                let rhs = self.expr(OR_PREC + 1)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Or(e.into(), rhs.into()),
                })
            }
            _ => Err(BasicError {
                marks: vec![mark],
                message: format!("Expected infix operator"),
                help: None,
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
                let desc = ExprDesc::String((*s).into());
                self.gettok();
                Ok(Expr { mark, desc })
            }
            Token::LBracket => {
                self.gettok();
                if self.consume(Token::RBracket) {
                    Ok(Expr {
                        mark,
                        desc: ExprDesc::List(vec![], None),
                    })
                } else {
                    let body = self.expr(0)?;
                    if self.consume(Token::Name("for")) {
                        let target = expr_to_assign_target(self.expr(UNARY_PREC)?)?;
                        self.expect(Token::Name("in"))?;
                        let container = self.expr(0)?;
                        let cond = if self.consume(Token::Name("if")) {
                            Some(self.expr(0)?.into())
                        } else {
                            None
                        };
                        self.expect(Token::RBracket)?;
                        Ok(Expr {
                            mark,
                            desc: ExprDesc::ListComprehension(
                                body.into(),
                                target.into(),
                                container.into(),
                                cond,
                            ),
                        })
                    } else {
                        let mut exprs = vec![body];
                        let mut variadic = None;
                        if self.consume(Token::Comma) {
                            while !self.consume(Token::RBracket) {
                                if self.consume(Token::Star) {
                                    variadic = Some(self.expr(0)?.into());
                                    self.expect(Token::RBracket)?;
                                    break;
                                }
                                exprs.push(self.expr(0)?);
                                if !self.consume(Token::Comma) {
                                    self.expect(Token::RBracket)?;
                                    break;
                                }
                            }
                        } else {
                            self.expect(Token::RBracket)?;
                        }
                        Ok(Expr {
                            mark,
                            desc: ExprDesc::List(exprs, variadic),
                        })
                    }
                }
            }
            Token::LBrace => {
                self.gettok();
                if self.consume(Token::Colon) {
                    self.expect(Token::RBrace)?;
                    Ok(Expr {
                        mark,
                        desc: ExprDesc::Map(vec![]),
                    })
                } else if self.consume(Token::RBrace) {
                    Ok(Expr {
                        mark,
                        desc: ExprDesc::Set(vec![]),
                    })
                } else {
                    let first = self.expr(0)?;
                    if self.consume(Token::Colon) {
                        let mut pairs = Vec::new();
                        let val = self.expr(0)?;
                        pairs.push((first, val));
                        if self.consume(Token::Comma) {
                            while !self.consume(Token::RBrace) {
                                let key = self.expr(0)?;
                                self.expect(Token::Colon)?;
                                let val = self.expr(0)?;
                                pairs.push((key, val));
                                if !self.consume(Token::Comma) {
                                    self.expect(Token::RBrace)?;
                                    break;
                                }
                            }
                        } else {
                            self.expect(Token::RBrace)?;
                        }
                        Ok(Expr {
                            mark,
                            desc: ExprDesc::Map(pairs),
                        })
                    } else {
                        let mut exprs = vec![first];
                        if self.consume(Token::Comma) {
                            while !self.consume(Token::RBrace) {
                                exprs.push(self.expr(0)?);
                                if !self.consume(Token::Comma) {
                                    self.expect(Token::RBrace)?;
                                    break;
                                }
                            }
                        } else {
                            self.expect(Token::RBrace)?;
                        }
                        Ok(Expr {
                            mark,
                            desc: ExprDesc::Set(exprs),
                        })
                    }
                }
            }
            Token::Minus | Token::Plus => {
                let tok = self.gettok();
                let op = match tok {
                    Token::Minus => Unop::Arithmetic(ArithmeticUnop::Negative),
                    Token::Plus => Unop::Arithmetic(ArithmeticUnop::Positive),
                    _ => panic!("parse unop: {:?}", tok),
                };
                let expr = self.expr(UNARY_PREC)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Unop(op, expr.into()),
                })
            }
            Token::Name("NOT") | Token::Name("not") => {
                self.gettok();
                let expr = self.expr(NOT_PREC)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Unop(Unop::Not, expr.into()),
                })
            }
            Token::Name("nil") => {
                self.gettok();
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Nil,
                })
            }
            Token::Name("true") => {
                self.gettok();
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Bool(true),
                })
            }
            Token::Name("false") => {
                self.gettok();
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Bool(false),
                })
            }
            Token::Name("yield") => {
                self.gettok();
                let yieldexpr = self.expr(0)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Yield(yieldexpr.into()),
                })
            }
            Token::Name("if") | Token::Name("IF") => {
                self.gettok();
                let cond = self.expr(0)?;
                if !self.consume(Token::Name("THEN")) {
                    self.expect(Token::Name("then"))?;
                }
                let body = self.expr(0)?;
                if !self.consume(Token::Name("ELSE")) {
                    self.expect(Token::Name("else"))?;
                }
                let other = self.expr(0)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::If(cond.into(), body.into(), other.into()),
                })
            }
            Token::Name("NEXT") => {
                self.gettok();
                self.expect(Token::LParen)?;
                let genexpr = self.expr(0)?;
                self.consume(Token::Comma);
                self.expect(Token::RParen)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Next(genexpr.into()),
                })
            }
            Token::Name("CAT") => {
                self.gettok();
                self.expect(Token::LParen)?;
                let exprs = self.args()?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Cat(exprs),
                })
            }
            Token::Name("DISASM") => {
                self.gettok();
                self.expect(Token::LParen)?;
                let fexpr = self.expr(0)?;
                self.consume(Token::Comma);
                self.expect(Token::RParen)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Disasm(fexpr.into()),
                })
            }
            Token::Name("SEND") => {
                self.gettok();
                self.expect(Token::LParen)?;
                let mut args = self.args()?;
                if args.is_empty() {
                    return Err(BasicError::new(
                        vec![mark],
                        format!("SEND requires at least a 'code' argument, but got no args"),
                    ));
                }
                let cexpr = args.remove(0).into();
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Send(cexpr, args),
                })
            }
            Token::Name(name) if self.tenop_map.contains_key(name) => {
                let op = *self.tenop_map.get(name).unwrap();
                self.gettok();
                self.expect(Token::LParen)?;
                let expr1 = self.expr(0)?;
                self.expect(Token::Comma)?;
                let expr2 = self.expr(0)?;
                self.expect(Token::Comma)?;
                let expr3 = self.expr(0)?;
                self.consume(Token::Comma);
                self.expect(Token::RParen)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Tenop(op, expr1.into(), expr2.into(), expr3.into()),
                })
            }
            Token::Name(name) if self.binop_map.contains_key(name) => {
                let op = *self.binop_map.get(name).unwrap();
                self.gettok();
                self.expect(Token::LParen)?;
                let expr1 = self.expr(0)?;
                self.expect(Token::Comma)?;
                let expr2 = self.expr(0)?;
                self.consume(Token::Comma);
                self.expect(Token::RParen)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Binop(op, expr1.into(), expr2.into()),
                })
            }
            Token::Name(name) if self.unop_map.contains_key(name) => {
                let op = *self.unop_map.get(name).unwrap();
                self.gettok();
                self.expect(Token::LParen)?;
                let expr = self.expr(0)?;
                self.consume(Token::Comma);
                self.expect(Token::RParen)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Unop(op, expr.into()),
                })
            }
            Token::Name(name) if self.zop_map.contains_key(name) => {
                let op = *self.zop_map.get(name).unwrap();
                self.gettok();
                self.expect(Token::LParen)?;
                self.expect(Token::RParen)?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::Zop(op),
                })
            }
            Token::Name(name) if !self.keywords.contains(name) => {
                let name = self.expect_name()?;
                Ok(Expr {
                    mark,
                    desc: ExprDesc::GetVar(name),
                })
            }
            Token::Name(name) if self.keywords.contains(name) => Err(BasicError {
                marks: vec![mark],
                message: format!("Expected expression but got keyword {:?}", name),
                help: None,
            }),
            _ => Err(BasicError {
                marks: vec![mark],
                message: format!("Expected expression but got {:?}", self.peek()),
                help: None,
            }),
        }
    }
    fn consume_delim(&mut self) {
        while self.at(Token::Newline) || self.at(Token::Semicolon) {
            self.gettok();
        }
    }
    fn at_delim(&self) -> bool {
        match self.peek() {
            Token::Name("end") | Token::EOF | Token::Semicolon | Token::Newline => true,
            _ => false,
        }
    }
    fn delim(&mut self) -> Result<(), BasicError> {
        if self.at_delim() {
            self.consume_delim();
            Ok(())
        } else {
            Err(BasicError {
                marks: vec![self.mark()],
                message: format!("Expected delimiter but got {:?}", self.peek()),
                help: None,
            })
        }
    }
    fn args(&mut self) -> Result<Vec<Expr>, BasicError> {
        let mut ret = Vec::new();
        while !self.consume(Token::RParen) {
            ret.push(self.expr(0)?);
            if !self.consume(Token::Comma) {
                self.expect(Token::RParen)?;
                break;
            }
        }
        Ok(ret)
    }
    fn args_with_kwargs(&mut self) -> Result<(Vec<Expr>, Vec<(RcStr, Expr)>), BasicError> {
        let mut args = Vec::new();
        let mut kwargs = Vec::new();

        while !self.consume(Token::RParen) {
            if self.at(Pat::Name) && self.lookahead(1) == Some(&Token::Eq) {
                let kw = self.expect_name()?;
                self.expect(Token::Eq)?;
                let arg = self.expr(0)?;
                kwargs.push((kw, arg));
            } else if kwargs.is_empty() {
                args.push(self.expr(0)?);
            } else {
                return Err(BasicError {
                    marks: vec![self.mark()],
                    message: format!("Positional arguments cannot come after keyword arguments"),
                    help: None,
                });
            }

            if !self.consume(Token::Comma) {
                self.expect(Token::RParen)?;
                break;
            }
        }

        Ok((args, kwargs))
    }
}

fn expr_to_assign_target(expr: Expr) -> Result<AssignTarget, BasicError> {
    match expr.desc {
        ExprDesc::GetVar(name) => Ok(AssignTarget {
            mark: expr.mark,
            desc: AssignTargetDesc::Name(name),
        }),
        ExprDesc::List(exprs, vtargetexpr) => {
            let mut targets = Vec::new();
            for expr in exprs {
                targets.push(expr_to_assign_target(expr)?);
            }
            let vtarget = if let Some(vtargetexpr) = vtargetexpr {
                Some(expr_to_assign_target(*vtargetexpr)?.into())
            } else {
                None
            };
            Ok(AssignTarget {
                mark: expr.mark,
                desc: AssignTargetDesc::List(targets, vtarget),
            })
        }
        ExprDesc::Binop(Binop::GetItem, owner, index) => Ok(AssignTarget {
            mark: expr.mark,
            desc: AssignTargetDesc::Subscript(*owner, *index),
        }),
        ExprDesc::GetAttr(owner, attr) => Ok(AssignTarget {
            mark: expr.mark,
            desc: AssignTargetDesc::Attribute(*owner, attr),
        }),
        _ => Err(BasicError {
            marks: vec![expr.mark],
            message: format!("The left hand side is not assignable"),
            help: Some(
                concat!(
                    "An assignable expression is either a name or a list of other assignable ",
                    "expressions. For example, 'x' or '[a, b, [x, y]]'",
                )
                .into(),
            ),
        }),
    }
}

fn precof<'a>(tok: &Token<'a>) -> Prec {
    match tok {
        Token::Name("is")
        | Token::Name("not")
        | Token::Name("in")
        | Token::Eq2
        | Token::Ne
        | Token::LessThan
        | Token::LessThanOrEqual
        | Token::GreaterThan
        | Token::GreaterThanOrEqual => CMP_PREC,
        Token::Minus | Token::Plus => ADD_PREC,
        Token::Star | Token::Slash | Token::Slash2 | Token::Percent => MUL_PREC,
        Token::Caret => POW_PREC,
        Token::LParen | Token::LBracket | Token::Dot | Token::RightArrow => POSTFIX_PREC,
        Token::Name("AND") | Token::Name("and") => AND_PREC,
        Token::Name("OR") | Token::Name("or") => OR_PREC,
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
