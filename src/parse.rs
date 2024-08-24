use crate::{
    lex::{Token, TokenKind},
    Lexer,
};
use miette::{Error, LabeledSpan, WrapErr};
use std::{borrow::Cow, fmt};

pub struct Parser<'de> {
    whole: &'de str,
    lexer: Lexer<'de>,
}

pub struct Ast;

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            lexer: Lexer::new(input),
        }
    }

    pub fn parse_expression(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_expression_within(0)
    }

    pub fn parse(mut self) -> Result<TokenTree<'de>, Error> {
        // TODO: in a loop
        self.parse_statement_within(0)
    }

    pub fn parse_block(&mut self) -> Result<TokenTree<'de>, Error> {
        self.lexer.expect(TokenKind::LeftBrace, "missing {")?;
        // TODO: in a loop with semicolons? depends on class vs body
        let block = self.parse_statement_within(0)?;
        self.lexer.expect(TokenKind::RightBrace, "missing }")?;

        Ok(block)
    }

    pub fn parse_fun_call_arguments(&mut self) -> Result<Vec<TokenTree<'de>>, Error> {
        let mut arguments = Vec::new();

        // parent has already eaten left paren as the operator

        if matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                kind: TokenKind::RightParen,
                ..
            }))
        ) {
            // immediate argument list end
        } else {
            loop {
                let argument = self.parse_expression_within(0).wrap_err_with(|| {
                    format!("in argument #{} of function call", arguments.len() + 1)
                })?;
                arguments.push(argument);

                let token = self
                    .lexer
                    .expect_where(
                        |token| matches!(token.kind, TokenKind::RightParen | TokenKind::Comma),
                        "continuing argument list",
                    )
                    .wrap_err("in argument list of function call")?;

                if token.kind == TokenKind::RightParen {
                    break;
                }
            }
        }

        Ok(arguments)
    }

    pub fn parse_statement_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                return Err(e).wrap_err("on the left-hand side");
            }
        };

        let mut lhs = match lhs {
            Token {
                kind: TokenKind::Ident,
                origin,
                ..
            } => TokenTree::Atom(Atom::Ident(origin)),

            Token {
                kind: TokenKind::Super,
                ..
            } => TokenTree::Atom(Atom::Super),

            Token {
                kind: TokenKind::This,
                ..
            } => TokenTree::Atom(Atom::This),

            Token {
                kind: TokenKind::LeftParen,
                ..
            } => {
                let lhs = self
                    .parse_expression_within(0)
                    .wrap_err("in bracketed expression")?;

                self.lexer
                    .expect(
                        TokenKind::RightParen,
                        "Unexpected end to bracketed expression",
                    )
                    .wrap_err("after bracketed expression")?;

                TokenTree::Cons(Op::Group, vec![lhs])
            }

            // unary prefix expressions
            Token {
                kind: TokenKind::Print | TokenKind::Return,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Print => Op::Print,
                    TokenKind::Return => Op::Return,
                    _ => unreachable!("by the outer match arm pattern"),
                };
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self
                    .parse_expression_within(r_bp)
                    .wrap_err_with(|| format!("on the right-hand side of {op:?}"))?;
                return Ok(TokenTree::Cons(op, vec![rhs]));
            }

            Token {
                kind: TokenKind::For,
                ..
            } => {
                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err("in for loop condition")?;

                let init = self
                    .parse_expression_within(0)
                    .wrap_err("in init condition of for loop")?;

                self.lexer
                    .expect(TokenKind::Semicolon, "missing ;")
                    .wrap_err("in for loop condition")?;

                let cond = self
                    .parse_expression_within(0)
                    .wrap_err("in loop condition of for loop")?;

                self.lexer
                    .expect(TokenKind::Semicolon, "missing ;")
                    .wrap_err("in for loop condition")?;

                let inc = self
                    .parse_expression_within(0)
                    .wrap_err("in incremental condition of for loop")?;

                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in for loop condition")?;

                let block = self.parse_block().wrap_err("in body of for loop")?;

                return Ok(TokenTree::Cons(Op::For, vec![init, cond, inc, block]));
            }

            Token {
                kind: TokenKind::While,
                ..
            } => {
                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err("in while loop condition")?;

                let cond = self
                    .parse_expression_within(0)
                    .wrap_err("in while loop condition")?;

                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in while loop condition")?;

                let block = self.parse_block().wrap_err("in body of while loop")?;

                return Ok(TokenTree::Cons(Op::While, vec![cond, block]));
            }

            Token {
                kind: TokenKind::Class,
                ..
            } => {
                let token = self
                    .lexer
                    .expect(TokenKind::Ident, "expected identifier")
                    .wrap_err("in class name")?;
                assert_eq!(token.kind, TokenKind::Ident);
                let ident = TokenTree::Atom(Atom::Ident(token.origin));

                if lhs.kind == TokenKind::Var {
                    self.lexer
                        .expect(TokenKind::Equal, "missing =")
                        .wrap_err("in variable assignment")?;
                }

                let block = self.parse_block().wrap_err("in class definition")?;

                return Ok(TokenTree::Cons(Op::Class, vec![ident, block]));
            }

            Token {
                kind: TokenKind::Var,
                ..
            } => {
                let token = self
                    .lexer
                    .expect(TokenKind::Ident, "expected identifier")
                    .wrap_err("in variable assignment")?;
                assert_eq!(token.kind, TokenKind::Ident);
                let ident = TokenTree::Atom(Atom::Ident(token.origin));

                self.lexer
                    .expect(TokenKind::Equal, "missing =")
                    .wrap_err("in variable assignment")?;

                let second = self
                    .parse_expression_within(0)
                    .wrap_err("in variable assignment expression")?;

                return Ok(TokenTree::Cons(Op::Var, vec![ident, second]));
            }

            Token {
                kind: TokenKind::Fun,
                ..
            } => {
                let token = self
                    .lexer
                    .expect(TokenKind::Ident, "expected identifier")
                    .wrap_err("in function name declaration")?;
                assert_eq!(token.kind, TokenKind::Ident);
                let name = token.origin;
                let ident = Atom::Ident(token.origin);

                let mut parameters = Vec::new();

                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        kind: TokenKind::RightParen,
                        ..
                    }))
                ) {
                    // immediate parameter list end
                } else {
                    loop {
                        let parameter = self
                            .lexer
                            .expect(TokenKind::Ident, "unexpected token")
                            .wrap_err_with(|| {
                                format!("in parameter #{} of function {name}", parameters.len() + 1)
                            })?;
                        parameters.push(parameter);

                        let token = self
                            .lexer
                            .expect_where(
                                |token| {
                                    matches!(token.kind, TokenKind::RightParen | TokenKind::Comma)
                                },
                                "continuing parameter list",
                            )
                            .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                        if token.kind == TokenKind::RightParen {
                            break;
                        }
                    }
                }

                let block = self
                    .parse_block()
                    .wrap_err_with(|| format!("in body of function {name}"))?;

                return Ok(TokenTree::Fun {
                    name: ident,
                    parameters,
                    body: Box::new(block),
                });
            }

            Token {
                kind: TokenKind::If,
                ..
            } => {
                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err("in if condition")?;

                let cond = self
                    .parse_expression_within(0)
                    .wrap_err("in if loop condition")?;

                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in if loop condition")?;

                let block = self.parse_block().wrap_err("in body of if")?;

                let mut otherwise = None;
                if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        kind: TokenKind::Else,
                        ..
                    }))
                ) {
                    self.lexer.next();

                    otherwise = Some(self.parse_block().wrap_err("in body of else")?);
                }

                return Ok(TokenTree::If {
                    condition: Box::new(cond),
                    yes: Box::new(block),
                    no: otherwise.map(Box::new),
                });
            }

            token => {
                return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(token.offset..token.offset + token.origin.len(), "here"),
                    ],
                    help = format!("Unexpected {token:?}"),
                    "Expected a statement",
                }
                .with_source_code(self.whole.to_string()))
            }
        };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                return Err(self
                    .lexer
                    .next()
                    .expect("checked Some above")
                    .expect_err("checked Err above"))
                .wrap_err("in place of expected operator");
            }
            let op = match op.map(|res| res.as_ref().expect("handled Err above")) {
                None => break,
                Some(Token {
                    kind: TokenKind::LeftParen,
                    ..
                }) => Op::Call,
                Some(Token {
                    kind: TokenKind::Dot,
                    ..
                }) => Op::Field,

                Some(token) => return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(token.offset..token.offset + token.origin.len(), "here"),
                    ],
                    help = format!("Unexpected {token:?}"),
                    "Expected an operator",
                }
                .with_source_code(self.whole.to_string())),
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                lhs = match op {
                    Op::Call => TokenTree::Call {
                        callee: Box::new(lhs),
                        arguments: self
                            .parse_fun_call_arguments()
                            .wrap_err("in function call arguments")?,
                    },
                    _ => TokenTree::Cons(op, vec![lhs]),
                };
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                let rhs = self
                    .parse_expression_within(r_bp)
                    .wrap_err_with(|| format!("on the right-hand side of {lhs} {op}"))?;
                lhs = TokenTree::Cons(op, vec![lhs, rhs]);
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    pub fn parse_expression_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                return Err(e).wrap_err("on left-hand side");
            }
        };
        let mut lhs =
            match lhs {
                // atoms
                Token {
                    kind: TokenKind::String,
                    origin,
                    ..
                } => TokenTree::Atom(Atom::String(Token::unescape(origin))),
                Token {
                    kind: TokenKind::Number(n),
                    ..
                } => TokenTree::Atom(Atom::Number(n)),
                Token {
                    kind: TokenKind::True,
                    ..
                } => TokenTree::Atom(Atom::Bool(true)),
                Token {
                    kind: TokenKind::False,
                    ..
                } => TokenTree::Atom(Atom::Bool(false)),
                Token {
                    kind: TokenKind::Nil,
                    ..
                } => TokenTree::Atom(Atom::Nil),
                Token {
                    kind: TokenKind::Ident,
                    origin,
                    ..
                } => TokenTree::Atom(Atom::Ident(origin)),
                Token {
                    kind: TokenKind::Super,
                    ..
                } => TokenTree::Atom(Atom::Super),

                Token {
                    kind: TokenKind::This,
                    ..
                } => TokenTree::Atom(Atom::This),

                // groups
                Token {
                    kind: TokenKind::LeftParen,
                    ..
                } => {
                    let lhs = self
                        .parse_expression_within(0)
                        .wrap_err("in bracketed expression")?;
                    self.lexer
                        .expect(
                            TokenKind::RightParen,
                            "Unexpected end to bracketed expression",
                        )
                        .wrap_err("after bracketed expression")?;
                    TokenTree::Cons(Op::Group, vec![lhs])
                }

                // unary prefix expressions
                Token {
                    kind: TokenKind::Bang | TokenKind::Minus,
                    ..
                } => {
                    let op = match lhs.kind {
                        TokenKind::Bang => Op::Bang,
                        TokenKind::Minus => Op::Minus,
                        _ => unreachable!("by the outer match arm pattern"),
                    };
                    let ((), r_bp) = prefix_binding_power(op);
                    let rhs = self
                        .parse_expression_within(r_bp)
                        .wrap_err("in right-hand side")?;
                    TokenTree::Cons(op, vec![rhs])
                }

                token => return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(token.offset..token.offset + token.origin.len(), "here"),
                    ],
                    help = format!("Unexpected {token:?}"),
                    "Expected an expression",
                }
                .with_source_code(self.whole.to_string())),
            };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                return Err(self
                    .lexer
                    .next()
                    .expect("checked Some above")
                    .expect_err("checked Err above"))
                .wrap_err("in place of expected operator");
            }
            let op = match op.map(|res| res.as_ref().expect("handled Err above")) {
                None => break,

                Some(Token {
                    kind:
                        TokenKind::RightParen
                        | TokenKind::Comma
                        | TokenKind::Semicolon
                        | TokenKind::RightBrace,
                    ..
                }) => break,
                Some(Token {
                    kind: TokenKind::LeftParen,
                    ..
                }) => Op::Call,
                Some(Token {
                    kind: TokenKind::Dot,
                    ..
                }) => Op::Field,
                Some(Token {
                    kind: TokenKind::Minus,
                    ..
                }) => Op::Minus,
                Some(Token {
                    kind: TokenKind::Plus,
                    ..
                }) => Op::Plus,
                Some(Token {
                    kind: TokenKind::Star,
                    ..
                }) => Op::Star,
                Some(Token {
                    kind: TokenKind::BangEqual,
                    ..
                }) => Op::BangEqual,
                Some(Token {
                    kind: TokenKind::EqualEqual,
                    ..
                }) => Op::EqualEqual,
                Some(Token {
                    kind: TokenKind::LessEqual,
                    ..
                }) => Op::LessEqual,
                Some(Token {
                    kind: TokenKind::GreaterEqual,
                    ..
                }) => Op::GreaterEqual,
                Some(Token {
                    kind: TokenKind::Less,
                    ..
                }) => Op::Less,
                Some(Token {
                    kind: TokenKind::Greater,
                    ..
                }) => Op::Greater,
                Some(Token {
                    kind: TokenKind::Slash,
                    ..
                }) => Op::Slash,
                Some(Token {
                    kind: TokenKind::And,
                    ..
                }) => Op::And,
                Some(Token {
                    kind: TokenKind::Or,
                    ..
                }) => Op::Or,

                Some(token) => return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(token.offset..token.offset + token.origin.len(), "here"),
                    ],
                    help = format!("Unexpected {token:?}"),
                    "Expected an infix operator",
                }
                .with_source_code(self.whole.to_string())),
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                lhs = match op {
                    Op::Call => TokenTree::Call {
                        callee: Box::new(lhs),
                        arguments: self
                            .parse_fun_call_arguments()
                            .wrap_err("in function call arguments")?,
                    },
                    _ => TokenTree::Cons(op, vec![lhs]),
                };
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                lhs = match op {
                    // TODO: ternary
                    // let mhs = self.parse_within(0);
                    // assert_eq!(lexer.next(), Token::Op(':'));
                    // let rhs = self.parse_within(r_bp);
                    // TokenTree::Cons(op, vec![lhs, mhs, rhs])
                    _ => {
                        let rhs = self
                            .parse_expression_within(r_bp)
                            .wrap_err_with(|| format!("on the right-hand side of {lhs} {op}"))?;
                        TokenTree::Cons(op, vec![lhs, rhs])
                    }
                };
                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'de> {
    String(Cow<'de, str>),
    Number(f64),
    Nil,
    Bool(bool),
    Ident(&'de str),
    Super,
    This,
}

impl fmt::Display for Atom<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // NOTE: this feels more correct:
            // Atom::String(s) => write!(f, "\"{s}\""),
            Atom::String(s) => write!(f, "{s}"),
            Atom::Number(n) => {
                if *n == n.trunc() {
                    // tests require that integers are printed as N.0
                    write!(f, "{n}.0")
                } else {
                    write!(f, "{n}")
                }
            }
            Atom::Nil => write!(f, "nil"),
            Atom::Bool(b) => write!(f, "{b:?}"),
            Atom::Ident(i) => write!(f, "{i}"),
            Atom::Super => write!(f, "super"),
            Atom::This => write!(f, "this"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Minus,
    Plus,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Bang,
    And,
    Or,
    Call,
    For,
    Class,
    Print,
    Return,
    Field,
    Var,
    While,
    Group,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Minus => "-",
                Op::Plus => "+",
                Op::Star => "*",
                Op::BangEqual => "!=",
                Op::EqualEqual => "==",
                Op::LessEqual => "<=",
                Op::GreaterEqual => ">=",
                Op::Less => "<",
                Op::Greater => ">",
                Op::Slash => "/",
                Op::Bang => "!",
                Op::And => "and",
                Op::Or => "or",
                Op::For => "for",
                Op::Class => "class",
                Op::Print => "print",
                Op::Return => "return",
                Op::Field => ".",
                Op::Var => "var",
                Op::While => "while",
                Op::Call => "call",
                Op::Group => "group",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTree<'de> {
    Atom(Atom<'de>),
    Cons(Op, Vec<TokenTree<'de>>),
    Fun {
        name: Atom<'de>,
        parameters: Vec<Token<'de>>,
        body: Box<TokenTree<'de>>,
    },
    Call {
        callee: Box<TokenTree<'de>>,
        arguments: Vec<TokenTree<'de>>,
    },
    If {
        condition: Box<TokenTree<'de>>,
        yes: Box<TokenTree<'de>>,
        no: Option<Box<TokenTree<'de>>>,
    },
}

impl fmt::Display for TokenTree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Atom(i) => write!(f, "{}", i),
            TokenTree::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {s}")?
                }
                write!(f, ")")
            }
            TokenTree::Fun {
                name,
                parameters,
                body,
            } => {
                write!(f, "(def {name}")?;
                for p in parameters {
                    write!(f, " {p}")?
                }
                write!(f, " {body})")
            }
            TokenTree::Call { callee, arguments } => {
                write!(f, "({callee}")?;
                for a in arguments {
                    write!(f, " {a}")?
                }
                write!(f, ")")
            }
            TokenTree::If { condition, yes, no } => {
                write!(f, "(if {condition} {yes}")?;
                if let Some(no) = no {
                    write!(f, " {no}")?
                }
                write!(f, ")")
            }
        }
    }
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Print | Op::Return => ((), 1),
        Op::Bang | Op::Minus => ((), 11),
        _ => panic!("bad op: {:?}", op),
    }
}

fn postfix_binding_power(op: Op) -> Option<(u8, ())> {
    let res = match op {
        Op::Call => (13, ()),
        _ => return None,
    };
    Some(res)
}

fn infix_binding_power(op: Op) -> Option<(u8, u8)> {
    let res = match op {
        // '=' => (2, 1),
        // '?' => (4, 3),
        Op::And | Op::Or => (3, 4),
        Op::BangEqual
        | Op::EqualEqual
        | Op::Less
        | Op::LessEqual
        | Op::Greater
        | Op::GreaterEqual => (5, 6),
        Op::Plus | Op::Minus => (7, 8),
        Op::Star | Op::Slash => (9, 10),
        Op::Field => (16, 15),
        _ => return None,
    };
    Some(res)
}
