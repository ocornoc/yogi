use super::*;
pub use pre_ast::{AssignStyle, Fragment, Var, VarKind};

type Precedence = u8;

mod precedence {
    use super::Precedence;
    pub const MISC: Precedence = Precedence::MAX - 1;
    pub const INC_DEC: Precedence = MISC - 1;
    pub const FACT: Precedence = INC_DEC - 1;
    pub const UNOP: Precedence = FACT - 1;
    pub const NEG: Precedence = UNOP - 1;
    pub const POW: Precedence = NEG - 1;
    pub const MUL_DIV: Precedence = POW - 1;
    pub const CMP: Precedence = MUL_DIV - 1;
    pub const ADD_SUB: Precedence = CMP - 1;
    pub const NOT: Precedence = ADD_SUB - 1;
    pub const OR: Precedence = NOT - 1;
    pub const AND: Precedence = OR - 1;
}

impl Var {
    fn get_unop(&self, prec: Precedence) -> Option<Unop> {
        match self.name.as_str() {
            _ if prec > precedence::UNOP || self.kind != VarKind::Local => None,
            "abs" => Some(Unop::Abs),
            "sqrt" => Some(Unop::Sqrt),
            "sin" => Some(Unop::Sin),
            "cos" => Some(Unop::Cos),
            "tan" => Some(Unop::Tan),
            "asin" => Some(Unop::Asin),
            "acos" => Some(Unop::Acos),
            "atan" => Some(Unop::Atan),
            "not" if prec <= precedence::NOT => Some(Unop::Not),
            _ => None,
        }
    }

    fn get_binop(&self, prec: Precedence) -> Option<Binop> {
        if self.kind != VarKind::Local {
            None
        } else if prec <= precedence::AND && self.name == "and" {
            Some(Binop::And)
        } else if prec <= precedence::OR && self.name == "or" {
            Some(Binop::Or)
        } else {
            None
        }
    }
}

impl pre_ast::Binop {
    fn get_unop(&self) -> Option<Unop> {
        if *self == pre_ast::Binop::Sub {
            Some(Unop::Neg)
        } else {
            None
        }
    }

    fn get_binop(&self, prec: Precedence) -> Option<Binop> {
        match self {
            pre_ast::Binop::Add if prec <= precedence::ADD_SUB => Some(Binop::Add),
            pre_ast::Binop::Sub if prec <= precedence::ADD_SUB => Some(Binop::Sub),
            pre_ast::Binop::Mul if prec <= precedence::MUL_DIV => Some(Binop::Mul),
            pre_ast::Binop::Div if prec <= precedence::MUL_DIV => Some(Binop::Div),
            pre_ast::Binop::Rem if prec <= precedence::MUL_DIV => Some(Binop::Rem),
            pre_ast::Binop::Pow if prec <= precedence::POW => Some(Binop::Pow),
            pre_ast::Binop::Lt if prec <= precedence::CMP => Some(Binop::Lt),
            pre_ast::Binop::Le if prec <= precedence::CMP => Some(Binop::Le),
            pre_ast::Binop::Eq if prec <= precedence::CMP => Some(Binop::Eq),
            pre_ast::Binop::Ge if prec <= precedence::CMP => Some(Binop::Ge),
            pre_ast::Binop::Gt if prec <= precedence::CMP => Some(Binop::Gt),
            pre_ast::Binop::Ne if prec <= precedence::CMP => Some(Binop::Ne),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unop {
    Abs,
    Sqrt,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Fact,
    Not,
    Neg,
}

impl Unop {
    fn prec(&self) -> Precedence {
        match self {
            Unop::Not => precedence::NOT,
            Unop::Neg => precedence::NEG,
            Unop::Fact => precedence::FACT,
            _ => precedence::UNOP,
        }
    }
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
    And,
    Or,
}

impl Binop {
    fn prec(&self) -> Precedence {
        match self {
            Binop::Add | Binop::Sub => precedence::ADD_SUB,
            Binop::Mul | Binop::Div | Binop::Rem => precedence::MUL_DIV,
            Binop::Pow => precedence::POW,
            Binop::Lt | Binop::Le | Binop::Eq | Binop::Ge | Binop::Gt | Binop::Ne =>
                precedence::CMP,
            Binop::And => precedence::AND,
            Binop::Or => precedence::OR,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    NumLit(Number),
    StrLit(YString),
    Var(Var),
    PreInc(Var),
    PreDec(Var),
    PostInc(Var),
    PostDec(Var),
    Unop(Unop, Box<Expr>),
    Binop(Box<Expr>, Binop, Box<Expr>),
}

impl Expr {
    fn prec(&self) -> Precedence {
        match self {
            Expr::PreInc(_) | Expr::PreDec(_) | Expr::PostInc(_) | Expr::PostDec(_) =>
                precedence::INC_DEC,
            Expr::Unop(unop, _) => unop.prec(),
            Expr::Binop(_, binop, _) => binop.prec(),
            _ => precedence::MISC,
        }
    }

    fn fact(&mut self) -> bool {
        if self.prec() < precedence::FACT {
            match self {
                Expr::Unop(_, r) | Expr::Binop(_, _, r) => {
                    let mut temp = Expr::NumLit(Number::ZERO);
                    std::mem::swap(&mut temp, r);
                    temp = Expr::Unop(Unop::Fact, Box::new(temp));
                    **r = temp;
                    false
                },
                _ => unreachable!(),
            }
        } else {
            false
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assign(Var, AssignStyle, Expr),
    IfThenElse(Expr, Vec<Statement>, Vec<Statement>),
    Goto(Expr),
    PreInc(Var),
    PreDec(Var),
    PostInc(Var),
    PostDec(Var),
}

type ParseResult<T> = Result<T, ParseErr>;

#[derive(Debug, Clone, Error)]
pub enum ParseErr {
    #[error("unexpected assign")]
    UnexpectedAssign,
    #[error("unexpected op")]
    UnexpectedOp,
    #[error("unexpected expr")]
    UnexpectedExpr,
    #[error("expected expr")]
    ExpectedExpr,
    #[error("expected variable")]
    ExpectedVar,
    #[error("expected 'then' for if statement'")]
    ExpectedThen,
    #[error("no 'else' or 'end' after 'if' ... 'then'")]
    UnfinishedIfThen,
    #[error("no 'end' after 'if' ... 'then' ... 'else'")]
    UnfinishedIfThenElse,
}

#[derive(Debug, Clone, Default)]
pub struct Line(pub Vec<Statement>);

impl TryFrom<pre_ast::Line> for Line {
    type Error = ParseErr;

    fn try_from(mut line: pre_ast::Line) -> ParseResult<Self> {
        let frags = &mut line.0;
        frags.reverse();
        let mut statements = Vec::with_capacity(frags.len());
        while let Some(frag) = frags.pop() {
            parse_frag(frag, frags, &mut statements)?;
        }
        Ok(Line(statements))
    }
}

fn parse_frag(
    frag: Fragment,
    frags: &mut Vec<Fragment>,
    statements: &mut Vec<Statement>,
) -> ParseResult<()> {
    let statement = match frag {
        Fragment::Assign(_) => Err(ParseErr::UnexpectedAssign),
        Fragment::Goto => parse_goto(frags),
        Fragment::If => parse_if(frags),
        Fragment::Var(v) => parse_from_var(frags, v),
        Fragment::Unop(_) | Fragment::Binop(_) => Err(ParseErr::UnexpectedOp),
        Fragment::Inc(col) => parse_pre_incdec_stmt(frags, true, col),
        Fragment::Dec(col) => parse_pre_incdec_stmt(frags, false, col),
        Fragment::StrLit(_) | Fragment::NumLit(_) | Fragment::LeftParen
        | Fragment::RightParen => Err(ParseErr::UnexpectedExpr),
    }?;
    statements.push(statement);
    Ok(())
}

fn parse_expr(frags: &mut Vec<Fragment>) -> ParseResult<Expr> {
    parse_expr_aux(frags, Precedence::MIN)
}

fn parse_expr_aux(frags: &mut Vec<Fragment>, prec: Precedence) -> ParseResult<Expr> {
    let mut buffer = parse_expr_fragment(frags, prec)?;
    while let Some(frag) = frags.last() {
        // JUST parse a binary operator with precedence >= prec
        match frag {
            Fragment::Binop(b) => if let Some(binop) = b.get_binop(prec) {
                frags.pop();
                buffer = Expr::Binop(
                    Box::new(buffer),
                    binop,
                    Box::new(parse_expr_aux(frags, prec + 1)?),
                );
            } else {
                break;
            },
            Fragment::Var(var) => if let Some(binop) = var.get_binop(prec) {
                frags.pop();
                buffer = Expr::Binop(
                    Box::new(buffer),
                    binop,
                    Box::new(parse_expr_aux(frags, prec + 1)?),
                );
            } else {
                break;
            },
            Fragment::StrLit(_) | Fragment::NumLit(_) => Err(ParseErr::UnexpectedExpr)?,
            &Fragment::Inc(col) => if let Some(expr) = parse_post_inc_expr(&buffer, col) {
                buffer = expr;
                frags.pop();
            } else {
                break;
            },
            &Fragment::Dec(col) => if let Expr::Var(ref var) = buffer {
                if col == var.end + 1 {
                    if let Expr::Var(var) = buffer {
                        buffer = Expr::PostDec(var);
                        frags.pop();
                    } else {
                        unsafe { std::hint::unreachable_unchecked() }
                    }
                } else {
                    frags.pop();
                    frags.push(Fragment::Binop(pre_ast::Binop::Sub));
                    frags.push(Fragment::Binop(pre_ast::Binop::Sub));
                }
            } else {
                frags.pop();
                frags.push(Fragment::Binop(pre_ast::Binop::Sub));
                frags.push(Fragment::Binop(pre_ast::Binop::Sub));
            },
            Fragment::Unop(pre_ast::Unop::Fact) => if prec <= precedence::FACT {
                frags.pop();
                if !buffer.fact() {
                    buffer = Expr::Unop(Unop::Fact, Box::new(buffer));
                }
            },
            Fragment::RightParen => {
                frags.pop();
                break;
            },
            Fragment::Goto | Fragment::If => break,
            Fragment::Assign(_) | Fragment::LeftParen => Err(ParseErr::ExpectedExpr)?,
        }
    }
    Ok(buffer)
}

fn parse_expr_fragment(frags: &mut Vec<Fragment>, prec: Precedence) -> ParseResult<Expr> {
    match frags.pop().ok_or(ParseErr::ExpectedExpr)? {
        Fragment::Var(var) => parse_var_unop(frags, var, prec),
        Fragment::StrLit(s) => Ok(Expr::StrLit(s)),
        Fragment::NumLit(n) => Ok(Expr::NumLit(n)),
        Fragment::Inc(col) if prec >= precedence::INC_DEC =>
            parse_pre_incdec_expr(frags, true, col),
        Fragment::Dec(col) if prec >= precedence::INC_DEC =>
            parse_pre_incdec_expr(frags, false, col),
        Fragment::Binop(binop) => if binop.get_unop() == Some(Unop::Neg) {
            parse_neg(frags)
        } else {
            Err(ParseErr::ExpectedExpr)
        },
        Fragment::LeftParen => parse_expr(frags),
        _ => {
            return Err(ParseErr::ExpectedExpr);
        },
    }
}

fn parse_pre_incdec_expr(frags: &mut Vec<Fragment>, inc: bool, col: u8) -> ParseResult<Expr> {
    if let Some(Fragment::Var(var)) = frags.pop() {
        if var.start == col + 1 {
            Ok(if inc {
                Expr::PreInc(var)
            } else {
                Expr::PreDec(var)
            })
        } else {
            Err(ParseErr::ExpectedVar)
        }
    } else {
        Err(ParseErr::ExpectedVar)
    }
}

/*
fn parse_pre_incdec_expr(frags: &mut Vec<Fragment>, inc: bool) -> Option<Expr> {
    if matches!(frags.last(), Some(Fragment::Var(_))) {
        if let Some(Fragment::Var(var)) = frags.pop() {
            Some(if inc {
                Expr::PreInc(var)
            } else {
                Expr::PreDec(var)
            })
        } else {
            unreachable!()
        }
    } else {
        None
    }
}
*/

fn parse_post_inc_expr(buffer: &Expr, col: u8) -> Option<Expr> {
    if let Expr::Var(var) = buffer {
        if col == var.end + 1 {
            Some(Expr::PostInc(var.clone()))
        } else {
            None
        }
    } else {
        None
    }
}

fn parse_var_unop(frags: &mut Vec<Fragment>, var: Var, prec: Precedence) -> ParseResult<Expr> {
    Ok(if let Some(unop) = var.get_unop(prec) {
        Expr::Unop(unop, Box::new(parse_expr_aux(frags, unop.prec())?))
    } else {
        Expr::Var(var)
    })
}

fn parse_neg(frags: &mut Vec<Fragment>) -> ParseResult<Expr> {
    Ok(Expr::Unop(Unop::Neg, Box::new(parse_expr_aux(frags, precedence::NEG + 1)?)))
}

fn parse_goto(frags: &mut Vec<Fragment>) -> ParseResult<Statement> {
    Ok(Statement::Goto(parse_expr(frags)?))
}

fn parse_pre_incdec_stmt(frags: &mut Vec<Fragment>, inc: bool, col: u8) -> ParseResult<Statement> {
    match frags.pop().ok_or(ParseErr::ExpectedVar)? {
        Fragment::Var(v) => if col == v.start - 2 {
            Ok(if inc {
                Statement::PreInc(v)
            } else {
                Statement::PreDec(v)
            })
        } else {
            Err(ParseErr::ExpectedVar)
        },
        _ => Err(ParseErr::ExpectedVar),
    }
}

fn parse_if(frags: &mut Vec<Fragment>) -> ParseResult<Statement> {
    let cond = parse_expr(frags)?;
    match frags.pop() {
        Some(Fragment::Var(v)) if v.name == "then" => Ok(()),
        _ => Err(ParseErr::ExpectedThen),
    }?;
    let mut then_statements = Vec::with_capacity(2);
    let mut else_statements = Vec::new();
    let ended;
    loop {
        match frags.pop().ok_or(ParseErr::UnfinishedIfThen)? {
            Fragment::Var(v) if v.name == "else" => {
                ended = false;
                break;
            },
            Fragment::Var(v) if v.name == "end" => {
                ended = true;
                break;
            },
            frag => parse_frag(frag, frags, &mut then_statements)?,
        }
    }
    if !ended {
        loop {
            match frags.pop().ok_or(ParseErr::UnfinishedIfThenElse)? {
                Fragment::Var(v) if v.name == "end" => break,
                frag => parse_frag(frag, frags, &mut else_statements)?,
            }
        }
    }
    Ok(Statement::IfThenElse(cond, then_statements, else_statements))
}

fn parse_from_var(frags: &mut Vec<Fragment>, var: Var) -> ParseResult<Statement> {
    match frags.pop().ok_or(ParseErr::UnexpectedExpr)? {
        Fragment::Assign(assign) => parse_assign(frags, var, assign),
        Fragment::Inc(col) if col == var.end + 1 => Ok(Statement::PostInc(var)),
        Fragment::Dec(col) if col == var.end + 1 => Ok(Statement::PostDec(var)),
        _ => Err(ParseErr::UnexpectedExpr),
    }
}

fn parse_assign(
    frags: &mut Vec<Fragment>,
    var: Var,
    assign: AssignStyle,
) -> ParseResult<Statement> {
    Ok(Statement::Assign(var, assign, parse_expr(frags)?))
}

#[derive(Debug, Clone, Default)]
pub struct Script(pub [Line; NUM_LINES]);

impl TryFrom<pre_ast::Script> for Script {
    type Error = ParseErr;

    fn try_from(script: pre_ast::Script) -> Result<Self, Self::Error> {
        let mut new = Script::default();

        for (i, line) in IntoIter::new(script.0).enumerate() {
            new.0[i] = line.try_into()?;
        }

        Ok(new)
    }
}
