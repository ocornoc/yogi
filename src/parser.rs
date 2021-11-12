use std::ops::*;
use anyhow::*;
use derive_more::{Deref, DerefMut};
use arith::{Number, YString};
use pest::{Parser, iterators::Pair};
use pest_derive::*;
use super::*;

#[derive(Parser)]
#[grammar = "yolol.pest"]
struct YololParser;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Ident {
    pub name: String,
    pub global: bool,
}

impl Ident {
   pub  fn local(name: &str) -> Self {
        Ident {
            name: name.to_lowercase(),
            global: false,
        }
    }

    pub fn global(name: &str) -> Self {
        Ident {
            name: name.to_lowercase(),
            global: true,
        }
    }

    fn parse<'a>(mut pairs: impl Iterator<Item = Pair<'a, Rule>>) -> Ident {
        let pair = pairs.next().unwrap();
        debug_assert_eq!(pairs.next(), None);
        Ident {
            name: match pair.as_rule() {
                Rule::global_ident => pair.as_str()[1..].to_lowercase(),
                Rule::local_ident => pair.as_str().to_lowercase(),
                r => unreachable!("parse error in Ident: {:?}", r),
            },
            global: pair.as_rule() == Rule::global_ident,
        }
    }
}

#[derive(Debug, Copy, PartialEq, Eq, Clone)]
pub enum Binop {
    And,
    Or,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    Ne,
    Le,
    Lt,
    Ge,
    Gt,
}

impl Binop {
    fn parse(pair: Pair<Rule>) -> Binop {
        match pair.as_str().to_ascii_lowercase().as_str() {
            "and" => Binop::And,
            "or" => Binop::Or,
            "+" => Binop::Add,
            "-" => Binop::Sub,
            "*" => Binop::Mul,
            "/" => Binop::Div,
            "%" => Binop::Mod,
            "^" => Binop::Pow,
            "==" => Binop::Eq,
            "!=" => Binop::Ne,
            "<=" => Binop::Le,
            "<" => Binop::Lt,
            ">=" => Binop::Ge,
            ">" => Binop::Gt,
            s => unreachable!("parse error in Binop: '{}'", s),
        }
    }
}

#[derive(Debug, Copy, PartialEq, Eq, Clone)]
pub enum Unop {
    Neg,
    Not,
    Abs,
    Sqrt,
    Fact,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
}

impl Unop {
    fn parse(pair: Pair<Rule>) -> Unop {
        match pair.as_str().to_ascii_lowercase().as_str() {
            "-" => Unop::Neg,
            "not" => Unop::Not,
            "abs" => Unop::Abs,
            "sqrt" => Unop::Sqrt,
            "!" => Unop::Fact,
            "sin" => Unop::Sin,
            "cos" => Unop::Cos,
            "tan" => Unop::Tan,
            "asin" => Unop::Asin,
            "acos" => Unop::Acos,
            "atan" => Unop::Atan,
            s => unreachable!("parse error in Unop: '{}'", s),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Incdec {
    pub inc: bool,
    pub ident: Ident,
}

impl Incdec {
    fn parse(first: Pair<Rule>, second: Pair<Rule>) -> Incdec {
        let inc = match (first.as_str(), second.as_str()) {
            ("++", _) | (_, "++") => true,
            ("--", _) | (_, "--") => false,
            _ => unreachable!("neither first nor second token was ++/--"),
        };
        Incdec {
            inc,
            ident: Ident::parse(if first.as_rule() == Rule::ident {
                first
            } else {
                second
            }.into_inner())
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Binop(Box<Expr>, Binop, Box<Expr>),
    Unop(Unop, Box<Expr>),
    Incdec(Incdec),
    Ident(Ident),
    Number(Number),
    String(YString),
}

impl Expr {
    fn unop_consume<'a>(mut self, pairs: impl DoubleEndedIterator<Item = Pair<'a, Rule>>) -> Self {
        for pair in pairs {
            self = Expr::Unop(Unop::parse(pair), self.into());
        }

        self
    }

    fn binop_consume<'a>(
        mut self,
        mut pairs: impl DoubleEndedIterator<Item = Pair<'a, Rule>>,
    ) -> Result<Self> {
        while let [Some(op), Some(arg)] = [pairs.next(), pairs.next()] {
            self = Expr::Binop(self.into(), Binop::parse(op), Expr::parse(arg)?.into());
        }

        Ok(self)
    }

    fn parse(pair: Pair<Rule>) -> Result<Expr> {
        match pair.as_rule() {
            Rule::expression_and | Rule::expression_or | Rule::expression_add
            | Rule::expression_order | Rule::expression_multiply => {
                let mut pairs = pair.into_inner();
                let first = Expr::parse(pairs.next().unwrap())?;
                first.binop_consume(pairs)
            },
            Rule::expression_exponent => {
                let mut pairs = pair.into_inner().rev();
                let mut last = Expr::parse(pairs.next().unwrap())?;

                while let [Some(op), Some(arg)] = [pairs.next(), pairs.next()] {
                    last = Expr::Binop(Expr::parse(arg)?.into(), Binop::parse(op), last.into());
                }
        
                Ok(last)
            },
            Rule::expression_keyword | Rule::expression_not | Rule::expression_neg => {
                let mut pairs = pair.into_inner().rev();
                let last = Expr::parse(pairs.next().unwrap())?;
                Ok(last.unop_consume(pairs))
            },
            Rule::expression_postfix => {
                let mut pairs = pair.into_inner();
                let mut first = Expr::parse(pairs.next().unwrap())?;
                if pairs.next().is_some() {
                    first = Expr::Unop(Unop::Fact, first.into());
                }
                Ok(first)
            },
            Rule::expression_ident => {
                let mut pairs = pair.into_inner();
                let first = pairs.next().unwrap();
                if first.as_rule() == Rule::value {
                    Expr::parse(first)
                } else {
                    Ok(Expr::Incdec(Incdec::parse(first, pairs.next().unwrap())))
                }
            },
            Rule::value => {
                let pair = pair.into_inner().next().unwrap();
                match pair.as_rule() {
                    Rule::string => Ok(Expr::String({
                        let mut new = String::new();
                        let mut escaped = false;
                        let s = pair.as_str();
                        for c in s[1..s.len() - 1].chars() {
                            if escaped {
                                new.push(match c {
                                    '\\' => '\\',
                                    'b' => '\x08',
                                    'f' => '\x0C',
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    '"' => '"',
                                    _ => unreachable!(),
                                });
                                escaped = false;
                            } else if c == '\\' {
                                escaped = true;
                            } else {
                                new.push(c);
                            }
                        }
                        new
                    }.into())),
                    Rule::number => Ok(Expr::Number(pair.as_str().parse()?)),
                    Rule::ident => Ok(Expr::Ident(Ident::parse(pair.into_inner()))),
                    _ => Expr::parse(pair),
                }
            },
            r => unreachable!("parse error in Expr: {:?}", r),
        }
    }
}

impl<T: Into<Number>> From<T> for Expr {
    fn from(t: T) -> Self {
        Expr::Number(t.into())
    }
}

impl From<&'_ str> for Expr {
    fn from(s: &str) -> Self {
        Expr::String(s.into())
    }
}

impl From<Ident> for Expr {
    fn from(i: Ident) -> Self {
        Expr::Ident(i)
    }
}

impl From<Incdec> for Expr {
    fn from(i: Incdec) -> Self {
        Expr::Incdec(i)
    }
}

macro_rules! binop_impl {
    ($trait:ident, $f:ident, $binop:tt) => {
        impl $trait for Expr {
            type Output = Self;
        
            fn $f(self, rhs: Self) -> Self::Output {
                Expr::Binop(self.into(), Binop::$binop, rhs.into())
            }
        }
    };
}

binop_impl!(BitAnd, bitand, And);
binop_impl!(BitOr, bitor, Or);
binop_impl!(Add, add, Add);
binop_impl!(Sub, sub, Sub);
binop_impl!(Mul, mul, Mul);
binop_impl!(Div, div, Div);
binop_impl!(Rem, rem, Mod);
binop_impl!(BitXor, bitxor, Pow);

macro_rules! unop_impl {
    ($trait:ident, $f:ident, $unnop:tt) => {
        impl $trait for Expr {
            type Output = Self;
        
            fn $f(self) -> Self::Output {
                Expr::Unop(Unop::$unnop, self.into())
            }
        }
    };
}

unop_impl!(Not, not, Not);
unop_impl!(Neg, neg, Neg);

#[derive(Debug, Copy, PartialEq, Eq, Clone)]
pub enum AssignOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

impl AssignOp {
    fn parse(pair: Pair<Rule>) -> Option<AssignOp> {
        Some(match pair.as_str() {
            "=" => {
                return None;
            },
            "+=" => AssignOp::Add,
            "-=" => AssignOp::Sub,
            "*=" => AssignOp::Mul,
            "/=" => AssignOp::Div,
            "%=" => AssignOp::Mod,
            "^=" => AssignOp::Pow,
            _ => unreachable!(),
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Goto(Expr),
    Ite(Expr, Vec<Statement>, Vec<Statement>),
    Incdec(Incdec),
    Assign(Ident, Option<AssignOp>, Expr),
}

impl Statement {
    fn parse_goto<'a>(mut pairs: impl Iterator<Item = Pair<'a, Rule>>) -> Result<Statement> {
        let pair = pairs.next().unwrap();
        debug_assert_eq!(pairs.next(), None);
        Ok(Statement::Goto(Expr::parse(pair)?))
    }

    fn parse_ite<'a>(mut pairs: impl Iterator<Item = Pair<'a, Rule>>) -> Result<Statement> {
        let condition = Expr::parse(pairs.next().unwrap())?;
        let mut then = Vec::with_capacity(4);

        while let Some(pair) = pairs.next() {
            if pair.as_rule() == Rule::else_kw {
                break
            } else {
                debug_assert_eq!(pair.as_rule(), Rule::statement);
                then.push(Statement::parse(pair.into_inner())?);
            }
        }

        let mut e = Vec::with_capacity(4);

        for pair in pairs {
            debug_assert_eq!(pair.as_rule(), Rule::statement);
            e.push(Statement::parse(pair.into_inner())?);
        }

        Ok(Statement::Ite(condition, then, e))
    }

    fn parse_assign<'a>(mut pairs: impl Iterator<Item = Pair<'a, Rule>>) -> Result<Statement> {
        let mut pair = pairs.next().unwrap();
        debug_assert_eq!(pair.as_rule(), Rule::ident);
        let ident = Ident::parse(pair.into_inner());
        pair = pairs.next().unwrap();
        debug_assert_eq!(pair.as_rule(), Rule::assign_op);
        let assign = AssignOp::parse(pair);
        pair = pairs.next().unwrap();
        debug_assert_eq!(pair.as_rule(), Rule::expression_and);
        let expr = Expr::parse(pair)?;
        debug_assert_eq!(pairs.next(), None);
        Ok(Statement::Assign(ident, assign, expr))
    }

    fn parse<'a>(mut pairs: impl Iterator<Item = Pair<'a, Rule>>) -> Result<Statement> {
        let pair = pairs.next().unwrap();
        debug_assert_eq!(pairs.next(), None);
        let rule = pair.as_rule();
        let mut pairs = pair.into_inner();

        match rule {
            Rule::goto => Statement::parse_goto(pairs),
            Rule::if_stmt => Statement::parse_ite(pairs),
            Rule::modify => Ok(Statement::Incdec(Incdec::parse(
                pairs.next().unwrap(),
                pairs.next().unwrap(),
            ))),
            Rule::assign => Statement::parse_assign(pairs),
            r => unreachable!("parse error in Statement: {:?}", r),
        }
    }
}

impl From<Incdec> for Statement {
    fn from(i: Incdec) -> Self {
        Statement::Incdec(i)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default, Deref, DerefMut)]
pub struct Line {
    #[deref]
    pub stmts: Vec<Statement>,
}

impl Line {
    fn parse<'a>(pairs: impl Iterator<Item = Pair<'a, Rule>>) -> Result<Line> {
        let mut stmts = Vec::with_capacity(20);

        for stmt in pairs {
            match stmt.as_rule() {
                Rule::statement => {
                    stmts.push(Statement::parse(stmt.into_inner())?);
                },
                Rule::EOI => break,
                r => unreachable!("parse error in Line: {:?}", r),
            }
        }

        Ok(Line {
            stmts
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Deref, DerefMut)]
pub struct Program {
    #[deref]
    pub lines: Vec<Line>,
}

impl Program {
    pub fn parse(s: &str) -> Result<Program> {
        let mut lines = Vec::with_capacity(20);

        for line in YololParser::parse(Rule::program, s)? {
            match line.as_rule() {
                Rule::line => {
                    let length = line.as_str().trim_end().len();
                    ensure!(length <= 70, "Line length too long: {} chars", length);
                    lines.push(Line::parse(line.into_inner())?);
                },
                Rule::EOI => break,
                r => unreachable!("parse error in Program: {:?}", r),
            }
        }

        ensure!(lines.len() <= 20, "Too many lines in program! Found {} of them.", lines.len());
        // ensure the program has exactly 20 lines
        lines.extend(std::iter::repeat(Line::default()).take(20 - lines.len()));

        Ok(Program {
            lines
        })
    }
}

impl Default for Program {
    fn default() -> Self {
        Program {
            lines: vec![Line::default(); 20],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_test() -> Result<()> {
        let program = Program::parse("")?;
        assert_eq!(program, Program::default());
        let program = Program::parse("
        
        ")?;
        assert_eq!(program, Program::default());
        Ok(())
    }

    #[test]
    fn simple_comment_test() -> Result<()> {
        let program = Program::parse("// WOW!
        
        // huh?
        // wow // crazy  ")?;
        assert_eq!(program, Program::default());
        Ok(())
    }

    #[test]
    fn assignment() -> Result<()> {
        let program = Program::parse("\
        x=2 y = 5 :abc = -2 a=a!=a :x+=1+2 :w-=3 f*=\"hi\"\n\
        a=c<=:d :_Wab323 %=b++
        ")?;
        assert_eq!(program[0].stmts, vec![
            Statement::Assign(Ident::local("x"), None, 2.into()),
            Statement::Assign(Ident::local("y"), None, 5.into()),
            Statement::Assign(Ident::global("abc"), None, (-2).into()),
            Statement::Assign(Ident::local("a"), None, Expr::Binop(
                Box::new(Ident::local("a").into()),
                Binop::Ne,
                Box::new(Ident::local("a").into()),
            )),
            Statement::Assign(Ident::global("x"), AssignOp::Add.into(),
                Expr::Number(1.into()) + 2.into()
            ),
            Statement::Assign(Ident::global("w"), AssignOp::Sub.into(), 3.into()),
            Statement::Assign(
                Ident::local("f"),
                AssignOp::Mul.into(),
                "hi".into(),
            ),
        ]);
        assert_eq!(program[1].stmts, vec![
            Statement::Assign(Ident::local("a"), None, Expr::Binop(
                Box::new(Ident::local("c").into()),
                Binop::Le,
                Box::new(Ident::global("d").into()),
            )),
            Statement::Assign(Ident::global("_Wab323"), AssignOp::Mod.into(), Incdec {
                inc: true,
                ident: Ident::local("b"),
            }.into()),
        ]);
        Ok(())
    }

    #[test]
    fn if_then_else() -> Result<()> {
        let program = Program::parse("\
        if 2 then x = 22 else goto 2 end
        if x==2 then goto x++ goto 3 / 4 else y -= x--end
        if 2*--_ then if x then x^=2 end else end
        ")?;
        assert_eq!(program[0].stmts, vec![Statement::Ite(
            2.into(),
            vec![Statement::Assign(Ident::local("x"), None, 22.into())],
            vec![Statement::Goto(2.into())],
        )]);
        assert_eq!(program[1].stmts, vec![Statement::Ite(
            Expr::Binop(Box::new(Ident::local("x").into()), Binop::Eq, Box::new(2.into())),
            vec![
                Statement::Goto(Incdec {
                    inc: true,
                    ident: Ident::local("x"),
                }.into()),
                Statement::Goto(Expr::from(3) / 4.into()),
            ],
            vec![Statement::Assign(Ident::local("y"), AssignOp::Sub.into(), Incdec {
                inc: false,
                ident: Ident::local("x"),
            }.into())],
        )]);
        assert_eq!(program[2].stmts, vec![Statement::Ite(
            Expr::from(2) * Incdec {
                inc: false,
                ident: Ident::local("_"),
            }.into(),
            vec![Statement::Ite(Ident::local("x").into(),
                vec![Statement::Assign(Ident::local("x").into(), AssignOp::Pow.into(), 2.into())],
                vec![],
            )],
            vec![],
        )]);
        Ok(())
    }

    #[test]
    fn inc_stmt_test() -> Result<()> {
        let program = Program::parse("\
        x-- x++ ++x --x
        ")?;
        let base = Incdec {
            inc: true,
            ident: Ident::local("x"),
        };
        assert_eq!(program[0].stmts, vec![
            Incdec {
                inc: false,
                ..base.clone()
            }.into(),
            base.clone().into(),
            base.clone().into(),
            Incdec {
                inc: false,
                ..base.clone()
            }.into(),
        ]);
        Ok(())
    }

    #[test]
    fn assoc_test() -> Result<()> {
        let program = Program::parse("\
        y=a+b+c+(d+e)
        y=a*b*c*(d*e)
        y=(a^b)^c^d^e
        ")?;
        let y = Ident::local("y");
        let a: Expr = Ident::local("a").into();
        let b: Expr = Ident::local("b").into();
        let c: Expr = Ident::local("c").into();
        let d: Expr = Ident::local("d").into();
        let e: Expr = Ident::local("e").into();
        assert_eq!(program[0].stmts, vec![Statement::Assign(
            y.clone(),
            None,
            ((a.clone() + b.clone()) + c.clone()) + (d.clone() + e.clone()),
        )]);
        assert_eq!(program[1].stmts, vec![Statement::Assign(
            Ident::local("y"),
            None,
            ((a.clone() * b.clone()) * c.clone()) * (d.clone() * e.clone()),
        )]);
        assert_eq!(program[2].stmts, vec![Statement::Assign(
            Ident::local("y"),
            None,
            (a.clone() ^ b.clone()) ^ (c.clone() ^ (d.clone() ^ e.clone())),
        )]);
        Ok(())
    }
}
