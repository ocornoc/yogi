use std::hash::*;
use super::*;
use cst::{Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unop {
    Fact,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    Lt,
    Le,
    Eq,
    Ge,
    Gt,
    Ne,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignStyle {
    Normal,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VarKind {
    Global,
    Local,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub kind: VarKind,
    pub name: String,
    pub start: u8,
    pub end: u8,
}

impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.name == other.name
    }
}

impl Eq for Var {}

impl Hash for Var {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.name.hash(state);
    }
}

#[derive(Debug, Clone)]
pub enum Fragment {
    Assign(AssignStyle),
    Goto,
    If,
    Var(Var),
    Unop(Unop),
    Binop(Binop),
    StrLit(YString),
    NumLit(Number),
    Inc(u8),
    Dec(u8),
    LeftParen,
    RightParen,
}

#[derive(Debug, Clone, Default)]
pub struct Line(pub Vec<Fragment>);

impl Line {
    fn parse_op(line: &mut cst::Line, token: Token, op: char) -> Result<Fragment, ParseErr> {
        match op {
            '(' => Ok(Fragment::LeftParen),
            ')' => Ok(Fragment::RightParen),
            // parse compound assignment, <=, ==, !=, or >=
            c if if let Some(&Token {
                end, kind: TokenKind::Operator('='),
            ..}) = line.0.last() {
                end == token.end + 1
            } else {
                false
            } => {
                line.0.pop();
                Ok(match c {
                    '<' => Fragment::Binop(Binop::Le),
                    '>' => Fragment::Binop(Binop::Ge),
                    '=' => Fragment::Binop(Binop::Eq),
                    '+' => Fragment::Assign(AssignStyle::Add),
                    '-' => Fragment::Assign(AssignStyle::Sub),
                    '*' => Fragment::Assign(AssignStyle::Mul),
                    '/' => Fragment::Assign(AssignStyle::Div),
                    '%' => Fragment::Assign(AssignStyle::Rem),
                    '^' => Fragment::Assign(AssignStyle::Pow),
                    '!' => Fragment::Binop(Binop::Ne),
                    _ => {
                        return Err(ParseErr::UnknownSymbol(token.end));
                    },
                })
            },
            '<' => Ok(Fragment::Binop(Binop::Lt)),
            '>' => Ok(Fragment::Binop(Binop::Gt)),
            // parses + and ++
            '+' => Ok(match line.0.last() {
                Some(&Token {
                    end, kind: TokenKind::Operator('+'),
                ..}) if end == token.end + 1 => {
                    line.0.pop();
                    Fragment::Inc(token.end as u8)
                },
                _ => Fragment::Binop(Binop::Add),
            }),
            // parses - and --
            '-' => Ok(match line.0.last() {
                Some(&Token {
                    end, kind: TokenKind::Operator('-'),
                ..}) if end == token.end + 1 => {
                    line.0.pop();
                    Fragment::Dec(token.end as u8)
                },
                _ => Fragment::Binop(Binop::Sub),
            }),
            '*' => Ok(Fragment::Binop(Binop::Mul)),
            '/' => Ok(Fragment::Binop(Binop::Div)),
            '%' => Ok(Fragment::Binop(Binop::Rem)),
            '^' => Ok(Fragment::Binop(Binop::Pow)),
            '!' => Ok(Fragment::Unop(Unop::Fact)),
            '=' => Ok(Fragment::Assign(AssignStyle::Normal)),
            _ => Err(ParseErr::UnknownSymbol(token.end)),
        }
    }
}

#[derive(Debug, Clone, Error)]
pub enum ParseErr {
    #[error("unknown symbol in column {0}")]
    UnknownSymbol(usize),
    #[error("{0}")]
    NumberParseError(<Number as FromStr>::Err),
}

impl From<<Number as FromStr>::Err> for ParseErr {
    fn from(n: <Number as FromStr>::Err) -> Self {
        ParseErr::NumberParseError(n)
    }
}

impl TryFrom<cst::Line> for Line {
    type Error = ParseErr;

    fn try_from(mut line: cst::Line) -> Result<Self, Self::Error> {
        line.0.reverse();
        let mut fragments = Vec::with_capacity(line.0.len());
        while let Some(token) = line.0.pop() {
            match token.kind {
                TokenKind::Operator(op) => {
                    fragments.push(Line::parse_op(&mut line, token, op)?);
                },
                TokenKind::LocalIdent(name) => {
                    fragments.push(Fragment::Var(Var {
                        kind: VarKind::Local,
                        start: token.start as u8,
                        end: token.end as u8,
                        name,
                    }));
                },
                TokenKind::GlobalIdent(name) => {
                    fragments.push(Fragment::Var(Var {
                        kind: VarKind::Global,
                        start: token.start as u8,
                        end: token.end as u8,
                        name,
                    }));
                },
                TokenKind::If => {
                    fragments.push(Fragment::If);
                },
                TokenKind::Goto => {
                    fragments.push(Fragment::Goto);
                },
                TokenKind::StrLit(s) => {
                    fragments.push(Fragment::StrLit(YString(s)));
                },
                TokenKind::NumLit(n) => {
                    fragments.push(Fragment::NumLit(n.parse()?));
                },
            }
        }
        Ok(Line(fragments))
    }
}

#[derive(Debug, Clone, Default)]
pub struct Script(pub [Line; NUM_LINES]);

impl TryFrom<cst::Script> for Script {
    type Error = ParseErr;

    fn try_from(script: cst::Script) -> Result<Self, Self::Error> {
        let mut new = Script::default();

        for (i, line) in IntoIter::new(script.0).enumerate() {
            new.0[i] = line.try_into()?;
        }

        Ok(new)
    }
}
