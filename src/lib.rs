use miette::{Diagnostic, Error, LabeledSpan, SourceSpan};
use std::{borrow::Cow, fmt};
use thiserror::Error;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected token '{token}' in input")]
pub struct SingleTokenError {
    #[source_code]
    src: String,

    pub token: char,

    #[label = "this input character"]
    err_span: SourceSpan,
}

impl SingleTokenError {
    pub fn line(&self) -> usize {
        let until_unrecongized = &self.src[..=self.err_span.offset()];
        until_unrecongized.lines().count()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'de> {
    origin: &'de str,
    kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Bang,
    Equal,
    String,
    Ident,
    Number(f64),
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let origin = self.origin;
        match self.kind {
            TokenKind::LeftParen => write!(f, "LEFT_PAREN {origin} null"),
            TokenKind::RightParen => write!(f, "RIGHT_PAREN {origin} null"),
            TokenKind::LeftBrace => write!(f, "LEFT_BRACE {origin} null"),
            TokenKind::RightBrace => write!(f, "RIGHT_BRACE {origin} null"),
            TokenKind::Comma => write!(f, "COMMA {origin} null"),
            TokenKind::Dot => write!(f, "DOT {origin} null"),
            TokenKind::Minus => write!(f, "MINUS {origin} null"),
            TokenKind::Plus => write!(f, "PLUS {origin} null"),
            TokenKind::Semicolon => write!(f, "SEMICOLON {origin} null"),
            TokenKind::Star => write!(f, "STAR {origin} null"),
            TokenKind::BangEqual => write!(f, "BANG_EQUAL {origin} null"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL {origin} null"),
            TokenKind::LessEqual => write!(f, "LESS_EQUAL {origin} null"),
            TokenKind::GreaterEqual => write!(f, "GREATER_EQUAL {origin} null"),
            TokenKind::Less => write!(f, "LESS {origin} null"),
            TokenKind::Greater => write!(f, "GREATER {origin} null"),
            TokenKind::Slash => write!(f, "SLASH {origin} null"),
            TokenKind::Bang => write!(f, "BANG {origin} null"),
            TokenKind::Equal => write!(f, "EQUAL {origin} null"),
            TokenKind::String => write!(f, "STRING {origin} {}", Token::unescape(origin)),
            TokenKind::Ident => write!(f, "IDENTIFIER {origin} null"),
            TokenKind::Number(n) => write!(f, "NUMBER {origin} {n}"),
            TokenKind::And => write!(f, "AND {origin} null"),
            TokenKind::Class => write!(f, "CLASS {origin} null"),
            TokenKind::Else => write!(f, "ELSE {origin} null"),
            TokenKind::False => write!(f, "FALSE {origin} null"),
            TokenKind::For => write!(f, "FOR {origin} null"),
            TokenKind::Fun => write!(f, "FUN {origin} null"),
            TokenKind::If => write!(f, "IF {origin} null"),
            TokenKind::Nil => write!(f, "NIL {origin} null"),
            TokenKind::Or => write!(f, "OR {origin} null"),
            TokenKind::Return => write!(f, "RETURN {origin} null"),
            TokenKind::Super => write!(f, "SUPER {origin} null"),
            TokenKind::This => write!(f, "THIS {origin} null"),
            TokenKind::True => write!(f, "TRUE {origin} null"),
            TokenKind::Var => write!(f, "VAR {origin} null"),
            TokenKind::While => write!(f, "WHILE {origin} null"),
        }
    }
}

impl Token<'_> {
    pub fn unescape<'de>(s: &'de str) -> Cow<'de, str> {
        todo!()
    }
}

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    /// Once the iterator returns `Err`, it will only return `None`.
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // NOTE: this must be in the loop for the indices to match up with c_onwards
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_str = &self.rest[..c.len_utf8()];
            let c_onwards = self.rest;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            enum Started {
                String,
                Number,
                Ident,
                IfEqualElse(TokenKind, TokenKind),
            }

            let just = move |kind: TokenKind| {
                Some(Ok(Token {
                    kind,
                    origin: c_str,
                }))
            };

            let started = match c {
                '(' => return just(TokenKind::LeftParen),
                ')' => return just(TokenKind::RightParen),
                '{' => return just(TokenKind::LeftBrace),
                '}' => return just(TokenKind::RightBrace),
                ',' => return just(TokenKind::Comma),
                '.' => return just(TokenKind::Dot),
                '-' => return just(TokenKind::Minus),
                '+' => return just(TokenKind::Plus),
                ';' => return just(TokenKind::Semicolon),
                '*' => return just(TokenKind::Star),
                '/' => return just(TokenKind::Slash),
                '<' => Started::IfEqualElse(TokenKind::LessEqual, TokenKind::Less),
                '>' => Started::IfEqualElse(TokenKind::GreaterEqual, TokenKind::Greater),
                '!' => Started::IfEqualElse(TokenKind::BangEqual, TokenKind::Bang),
                '=' => Started::IfEqualElse(TokenKind::EqualEqual, TokenKind::Equal),
                '"' => Started::String,
                '0'..='9' => Started::Number,
                'a'..='z' | 'A'..='Z' | '_' => Started::Ident,
                c if c.is_whitespace() => continue,
                c => {
                    return Some(Err(SingleTokenError {
                        src: self.whole.to_string(),
                        token: c,
                        err_span: SourceSpan::from(self.byte - c.len_utf8()..self.byte),
                    }
                    .into()));
                }
            };

            break match started {
                Started::String => todo!(),
                Started::Ident => {
                    let first_non_ident = c_onwards
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
                        .unwrap_or_else(|| c_onwards.len());

                    let literal = &c_onwards[..first_non_ident];
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let kind = match literal {
                        "and" => TokenKind::And,
                        "class" => TokenKind::Class,
                        "else" => TokenKind::Else,
                        "false" => TokenKind::False,
                        "for" => TokenKind::For,
                        "fun" => TokenKind::Fun,
                        "if" => TokenKind::If,
                        "nil" => TokenKind::Nil,
                        "or" => TokenKind::Or,
                        "return" => TokenKind::Return,
                        "super" => TokenKind::Super,
                        "this" => TokenKind::This,
                        "true" => TokenKind::True,
                        "var" => TokenKind::Var,
                        "while" => TokenKind::While,
                        _ => TokenKind::Ident,
                    };

                    return Some(Ok(Token {
                        origin: literal,
                        kind,
                    }));
                }
                Started::Number => {
                    let first_non_digit = c_onwards
                        .find(|c| !matches!(c, '.' | '0'..='9'))
                        .unwrap_or_else(|| c_onwards.len());

                    let mut literal = &c_onwards[..first_non_digit];
                    let mut dotted = literal.splitn(3, '.');
                    match (dotted.next(), dotted.next(), dotted.next()) {
                        (Some(one), Some(two), Some(_)) => {
                            literal = &literal[..one.len() + 1 + two.len()];
                        }
                        (Some(one), Some(two), None) if two.is_empty() => {
                            literal = &literal[..one.len()];
                        }
                        _ => {
                            // leave literal as-is
                        }
                    }
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let n = match literal.parse() {
                        Ok(n) => n,
                        Err(e) => {
                            return Some(Err(miette::miette! {
                                labels = vec![
                                    LabeledSpan::at(self.byte - literal.len()..self.byte, "this numeric literal"),
                                ],
                                "{e}",
                            }.with_source_code(self.whole.to_string())));
                        }
                    };

                    return Some(Ok(Token {
                        origin: literal,
                        kind: TokenKind::Number(n),
                    }));
                }
                Started::IfEqualElse(yes, no) => {
                    self.rest = self.rest.trim_start();
                    let trimmed = c_onwards.len() - self.rest.len() - 1;
                    self.byte += trimmed;
                    if self.rest.starts_with('=') {
                        let span = &c_onwards[..c.len_utf8() + trimmed + 1];
                        self.rest = &self.rest[1..];
                        self.byte += 1;
                        Some(Ok(Token {
                            origin: span,
                            kind: yes,
                        }))
                    } else {
                        Some(Ok(Token {
                            origin: c_str,
                            kind: no,
                        }))
                    }
                }
            };
        }
    }
}
