use anyhow::Result;
use super::*;
use parser::*;
use arith::*;
use ahash::AHashMap;

#[derive(Debug, Clone, Copy)]
enum ExecuteErr {
    RuntimeErr,
    Goto(usize),
}

type ExecuteResult<T> = Result<T, ExecuteErr>;

impl ExecuteErr {
    fn from_option<T>(option: Option<T>) -> ExecuteResult<T> {
        if let Some(s) = option {
            Ok(s)
        } else {
            Err(ExecuteErr::RuntimeErr)
        }
    }
}

impl From<RuntimeErr> for ExecuteErr {
    fn from(_: RuntimeErr) -> Self {
        ExecuteErr::RuntimeErr
    }
}

#[derive(Debug, Clone)]
pub struct SimpleInterp {
    values: AHashMap<Ident, Value>,
    line: usize,
    ast: Program,
}

impl From<Program> for SimpleInterp {
    fn from(ast: Program) -> Self {
        SimpleInterp {
            values: AHashMap::default(),
            line: 0,
            ast,
        }
    }
}

impl SimpleInterp {
    pub fn new(ast: Program) -> Self {
        ast.into()
    }

    fn eval_incdec(values: &mut AHashMap<Ident, Value>, incdec: &Incdec) -> ExecuteResult<Value> {
        let entry = values
            .entry(incdec.ident.clone())
            .or_default();
        if incdec.inc {
            entry.pre_inc();
        } else {
            entry.pre_dec()?;
        }
        Ok(entry.clone())
    }

    fn eval_expr(values: &mut AHashMap<Ident, Value>, expr: &Expr) -> ExecuteResult<Value> {
        match expr {
            &Expr::Binop(ref l, op, ref r) => {
                let r = Self::eval_expr(values, r)?;
                let mut l = Self::eval_expr(values, l)?;
                Ok(match op {
                    Binop::And => Value::Number((l.as_bool() && r.as_bool()).into()),
                    Binop::Or => Value::Number((l.as_bool() || r.as_bool()).into()),
                    Binop::Add => {
                        l += &r;
                        l
                    },
                    Binop::Sub => {
                        l -= &r;
                        l
                    },
                    Binop::Mul => (
                        ExecuteErr::from_option(l.as_number())?
                        * ExecuteErr::from_option(r.as_number())?
                    ).into(),
                    Binop::Div => (
                        ExecuteErr::from_option(l.as_number())?
                        / ExecuteErr::from_option(r.as_number())?
                    )?.into(),
                    Binop::Mod => (
                        ExecuteErr::from_option(l.as_number())?
                        % ExecuteErr::from_option(r.as_number())?
                    )?.into(),
                    Binop::Pow => ExecuteErr::from_option(l.as_number())?
                        .pow(ExecuteErr::from_option(r.as_number())?)
                        .into(),
                    Binop::Eq => Value::Number((l == r).into()),
                    Binop::Ne => Value::Number((l != r).into()),
                    Binop::Le => Value::Number((l <= r).into()),
                    Binop::Lt => Value::Number((l < r).into()),
                    Binop::Ge => Value::Number((l >= r).into()),
                    Binop::Gt => Value::Number((l > r).into()),
                })
            },
            &Expr::Unop(op, ref expr) => {
                let val = Self::eval_expr(values, expr)?;
                if op == Unop::Not {
                    return Ok((!val).into());
                }
                let n = ExecuteErr::from_option(val.as_number())?;
                Ok(match op {
                    Unop::Neg => -n,
                    Unop::Not => unreachable!(),
                    Unop::Abs => n.abs(),
                    Unop::Sqrt => n.sqrt(),
                    Unop::Fact => n.fact(),
                    Unop::Sin => n.sin(),
                    Unop::Cos => n.cos(),
                    Unop::Tan => n.tan(),
                    Unop::Asin => n.asin(),
                    Unop::Acos => n.acos(),
                    Unop::Atan => n.atan(),
                }.into())
            },
            Expr::Incdec(incdec) => Self::eval_incdec(values, incdec),
            Expr::Ident(ident) => Ok(values.entry(ident.clone()).or_default().clone()),
            Expr::Number(n) => Ok(n.clone().into()),
            Expr::String(s) => Ok(s.clone().into()),
        }
    }

    fn step_stmt(
        line: usize,
        values: &mut AHashMap<Ident, Value>,
        stmt: &Statement,
    ) -> ExecuteResult<()> {
        match stmt {
            Statement::Goto(expr) => {
                let number = ExecuteErr::from_option(Self::eval_expr(values, expr)?.as_number())?;
                let line = number.as_f32().floor().clamp(1.0, 20.0) as usize;
                Err(ExecuteErr::Goto(line - 1))
            },
            Statement::Ite(i, t, e) => {
                let stmts = if Self::eval_expr(values, i)?.as_bool() {
                    t
                } else {
                    e
                };
                Self::step_stmts(line, values, stmts)
            },
            Statement::Incdec(incdec) => Self::eval_incdec(values, incdec).map(|_| ()),
            Statement::Assign(id, op, expr) => {
                let val = Self::eval_expr(values, expr)?;
                let entry = values
                    .entry(id.clone())
                    .or_default();
                match op {
                    Some(AssignOp::Add) => {
                        *entry += &val;
                    },
                    Some(AssignOp::Sub) => {
                        *entry -= &val;
                    },
                    Some(AssignOp::Mul) => {
                        let entry = ExecuteErr::from_option(entry.as_number_mut())?;
                        *entry *= ExecuteErr::from_option(val.as_number())?;
                    },
                    Some(AssignOp::Div) => ExecuteErr::from_option(entry.as_number_mut())?
                        .div_assign(ExecuteErr::from_option(val.as_number())?)?,
                    Some(AssignOp::Mod) => ExecuteErr::from_option(entry.as_number_mut())?
                        .rem_assign(ExecuteErr::from_option(val.as_number())?)?,
                    Some(AssignOp::Pow) => ExecuteErr::from_option(entry.as_number_mut())?
                        .pow_assign(ExecuteErr::from_option(val.as_number())?),
                    None => {
                        *entry = val;
                    },
                }
                Ok(())
            },
        }
    }

    fn step_stmts(
        line: usize,
        values: &mut AHashMap<Ident, Value>,
        stmts: &[Statement],
    ) -> ExecuteResult<()> {
        for stmt in stmts {
            Self::step_stmt(line, values, stmt)?;
        }

        Ok(())
    }

    pub fn step_line(&mut self) {
        let line = &self.ast[self.line];
        self.line = match Self::step_stmts(self.line, &mut self.values, line) {
            Ok(_) | Err(ExecuteErr::RuntimeErr) => self.line + 1,
            Err(ExecuteErr::Goto(line)) => line,
        };
    }

    pub fn step_lines(&mut self, lines: usize) {
        for _ in 0..lines {
            self.step_line();
        }
    }

    /// Get a reference to the simple interp's values.
    pub const fn values(&self) -> &AHashMap<Ident, Value> {
        &self.values
    }

    /// Get a reference to the simple interp's line.
    pub const fn line(&self) -> usize {
        self.line
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tester(should_pass: bool, src: &str) {
        let mut interp = SimpleInterp::new(Program::parse(src).unwrap());
        interp.step_lines(10_000);
        if should_pass {
            assert_eq!(interp.values()[&Ident::global("output")], Value::Str("ok".into()));
        } else {
            assert_ne!(interp.values()[&Ident::global("output")], Value::Str("ok".into()));
        }
    }

    #[test]
    fn acid_acos() {
        tester(true,
r#"x=acos(0)
u/=x!=90 :OUTPUT="Failed #1 : " + x goto10
x=acos(1)
u/=x!=0 :OUTPUT="Failed #2 : " + x goto10
x=acos(0.5)
u/=x!=60 :OUTPUT="Failed #3 : " + x goto10
x=acos(17)
u/=x!=-9223372036854775.808 :OUTPUT="Failed #4 : " + x goto10
:OUTPUT="ok"
goto10"#
        );
    }

    #[test]
    fn acid_asin() {
        tester(true,
r#"x=asin(0)
u/=x!=0 :OUTPUT="Failed #1 : " + x goto10
x=asin(1)
u/=x!=90 :OUTPUT="Failed #2 : " + x goto10
x=asin(0.5)
u/=x!=30 :OUTPUT="Failed #3 : " + x goto10
x=asin(17)
u/=x!=-9223372036854775.808 :OUTPUT="Failed #4 : " + x goto10
:OUTPUT="ok"
goto10"#
        );
    }

    #[test]
    fn acid_atan() {
        tester(true,
r#"x=atan(0)
u/=x!=0 :OUTPUT="Failed #1 : " + x goto10
x=atan(1)
u/=x!=45 :OUTPUT="Failed #2 : " + x goto10
x=atan(0.5)
u/=x!=26.565 :OUTPUT="Failed #3 : " + x goto10
x=atan(998877665544332)
u/=x!=90 :OUTPUT="Failed #4 : " + x goto10
:OUTPUT="ok"
goto10"#
        );
    }

    #[test]
    fn acid_exponents() {
        tester(true,
r#"x=2^4
u/=x!=16 :OUTPUT="Failed #1 : " + x goto12
x=2^40
u/=x!=1099511627776 :OUTPUT="Failed #2 : " + x goto12
x=2^70
u/=x!=-9223372036854775.808 :OUTPUT="Failed #3 : " + x goto12
x=2^71
u/=x!=-9223372036854775.808 :OUTPUT="Failed #4 : " + x goto12
x=17^17
u/=x!=-9223372036854775.808 :OUTPUT="Failed #5 : " + x goto12
:OUTPUT="ok"
goto12"#
        );
    }

    #[test]
    fn acid_modulus() {
        tester(true,
r#"n=1 x=10%7 y=3 if x!=y then goto19 end n++ 
x=10%3 y=1 if x!=y then goto19 end n++ 
x=10%3.1 y=0.7 if x!=y then goto19 end n++ 
x=10%-3 y=1 if x!=y then goto19 end n++ 
x=10%(-3) y=1 if x!=y then goto19 end n++ 
x=10%-3.1 y=0.7 if x!=y then goto19 end n++
x=10%0.7 y=0.2 if x!=y then goto19 end n++
x=10%-0.7 y=0.2 if x!=y then goto19 end n++








if n != 9 then :OUTPUT="Skipped: "+(9-n)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+n+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_multiply() {
        tester(true,
r#"x=1 i=24 j=38 x*=3 x*=3 x*=3 x*=3 x*=3 
u/=x!=243 :OUTPUT="Failed #1 : " + x goto8
u/=i>0 i-- x*=3 goto3
u/=x!=-5156598929955.207 :OUTPUT="Failed #2 : " + x goto8
u/=j>0 j-- x*=3 goto5
u/=x!=-113000154446.553 :OUTPUT="Failed #2 : " + x goto8
:OUTPUT="ok"
goto8"#
        );
    }

    #[test]
    fn acid_precedence1() {
        tester(true,
r#"num=1 x=(0 and 0 or 1 ) y=0 if x!=y then goto19 end num++
x=((0 and 0) or 1 ) y= 1 if x!=y then goto19 end num++ 
x=(0 and (0 or 1) ) y= 0 if x!=y then goto19 end num++ 
x=(5+5-6 ) y= 4 if x!=y then goto19 end num++ 
x=(-6+5+5 ) y= 4 if x!=y then goto19 end num++ 
x=(5-6+5 ) y= 4 if x!=y then goto19 end num++ 
x=(2*5/4 ) y=2.5 if x!=y then goto19 end num++ 
x=(10/(2*4) ) y= 1.25 if x!=y then goto19 end num++ 
x=(10/2*4 ) y= 20 if x!=y then goto19 end num++ 
x=(2+2*2 ) y= 6 if x!=y then goto19 end num++ 
a=1 x=(5*a++ ) y= 10 if x!=y then goto19 end num++ 
a=2 x=(5*a-- ) y= 5 if x!=y then goto19 end num++ 
a=2 x=(-a++ ) y= -3 if x!=y then goto19 end num++ 
a=2 x=(-a! ) y= -2  if x!=y then goto19 end num++ 
a=2 x=(-(a!) ) y= -2 if x!=y then goto19 end num++ 

if num != 16 then :OUTPUT="Skipped: "+(16-num)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_precedence2() {
        tester(true,
r#"num=1 x=(sqrt 3! ) y=2.449 if x!=y then goto19 end num++ 
x=(sqrt (3!) ) y=2.449 if x!=y then goto19 end num++ 
x=((sqrt 9) ) y=3 if x!=y then goto19 end num++ 
x=((abs 3) ) y=3 if x!=y then goto19 end num++ 
a=2+2 x=(a! ) y=24  if x!=y then goto19 end num++ 
x=(2+3! ) y=8 if x!=y then goto19 end num++ 
x=(2*3! ) y=12 if x!=y then goto19 end num++ 
a=-3 x=(a! ) y=-9223372036854775.808 if x!=y then goto19 end num++ 
a=-3 x=(abs a! ) y=-9223372036854775.808 if x!=y then goto19 end num++ 
a=-3 x=(abs (a!) ) if x!=y then goto19 end num++ 






if num != 11 then :OUTPUT="Skipped: "+(11-num)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_precedence3() {
        tester(true,
r#"num=1 x=(2*2^2 ) y= 8 if x!=y then goto19 end num++ 
x=(2+2^2 ) y= 6 if x!=y then goto19 end num++ 
x=(-2^2 ) y= 4 if x!=y then goto19 end num++ 
x=(-(2^2) ) y= -4 if x!=y then goto19 end num++ 
x=(sqrt 3+6 ) y= 7.732 if x!=y then goto19 end num++ 
x=(sqrt (3+6) ) y= 3 if x!=y then goto19 end num++ 
x=(sqrt 3*3 ) y= 5.196 if x!=y then goto19 end num++ 
x=(abs -5+5 ) y= 10 if x!=y then goto19 end num++ 
x=(abs (-5+5) ) y= 0 if x!=y then goto19 end num++ 
x=(sin (1^2) ) y= 0.017 if x!=y then goto19 end num++ 
x=((sin 1)^2 ) y= 0 if x!=y then goto19 end num++ 
x=(sin 1^2 ) y= 0 if x!=y then goto19 end num++ 
x=(2+2>1+1 ) y= 4 if x!=y then goto19 end num++ 
x=(2+2>=1+1 ) y= 4 if x!=y then goto19 end num++ 
x=(2*2>1*1 ) y= 1 if x!=y then goto19 end num++ 
x=(2*2>=1*1 ) y= 1 if x!=y then goto19 end num++ 
if num != 17 then :OUTPUT="Skipped: "+(17-num)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_precedence4() {
        tester(true,
r#"num=1 x=(2*(2>1)*1 ) y= 2 if x!=y then goto19 end num++ 
x=(2^2>1^1 ) y= 1 if x!=y then goto19 end num++ 
x=(2+1==1+2 ) y= 5 if x!=y then goto19 end num++ 
x=(2*1==1*2 ) y= 1 if x!=y then goto19 end num++ 
x=(0==1>1==1 ) y= 0 if x!=y then goto19 end num++ 
x=((0==1)>(1==1) ) y= 0 if x!=y then goto19 end num++ 
x=(0==(1>1)==1 ) y= 1 if x!=y then goto19 end num++ 
x=((((0==1)>1)==1) ) y= 0 if x!=y then goto19 end num++ 
x=(0>1==0 ) y= 1 if x!=y then goto19 end num++ 
x=((0>1)==0 ) y= 1 if x!=y then goto19 end num++ 
x=(0>(1==0) ) y= 0 if x!=y then goto19 end num++ 
x=(0==(0 or 1)==1 ) y= 0 if x!=y then goto19 end num++ 
x=(0==0 or 1==1 ) y= 1 if x!=y then goto19 end num++ 
x=(1 or 0 == 0 ) y= 1 if x!=y then goto19 end num++ 
x=((1 or 0) == 0 ) y= 0 if x!=y then goto19 end num++ 
x=(1 or (0 == 0) ) y= 1 if x!=y then goto19 end num++ 
if num != 17 then :OUTPUT="Skipped: "+(17-num)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_precedence5() {
        tester(true,
r#"num=1 x=(not 1+1 ) y=0 if x!=y then goto19 end num++ 
x=(not 0+1 ) y=0 if x!=y then goto19 end num++ 
x=(not 0+0 ) y=1 if x!=y then goto19 end num++ 
x=(not (1+1) ) y=0 if x!=y then goto19 end num++ 
x=((not 1)+1 ) y=1 if x!=y then goto19 end num++ 
x=((not 0)+1 ) y=2 if x!=y then goto19 end num++ 
x=(not (1 and 1) ) y=0 if x!=y then goto19 end num++ 
x=(not (1 and 0) ) y=1 if x!=y then goto19 end num++ 
x=((not 1) and 1 )  y=0 if x!=y then goto19 end num++ 
x=((not 0) and 1 ) y=1 if x!=y then goto19 end num++ 
x=((not 0) and 0 ) y=0 if x!=y then goto19 end num++ 
x=(1 and not 0 and 1) y=1 if x!=y then goto19 end num++ 
x=(1 and not 1 and 1) y=0 if x!=y then goto19 end num++ 
x=(1 and not 0 and 0) y=0 if x!=y then goto19 end num++ 
x=(1 and not (0 and 0)) y=1 if x!=y then goto19 end num++ 
x=(1 and not 0) y=1 if x!=y then goto19 end num++ 
if num != 17 then :OUTPUT="Skipped: "+(17-num)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_precedence6() {
        tester(true,
r#"num=1 x=not(not 0) y=0 ifx!=y thengoto19end num++ 
x=not(not 1) y=1 ifx!=y thengoto19end num++ 
x=(not 0) and not 0 y=1 ifx!=y thengoto19end num++ 
x=(not 0) and not 1 y=0 ifx!=y thengoto19end num++ 
x=1+(not 1) y=1 ifx!=y thengoto19end num++ 
x=1+(not 0) y=2 ifx!=y thengoto19end num++ 
x=not 1+1 y=0 ifx!=y thengoto19end num++ 
x=not 0+1 y=0 ifx!=y thengoto19end num++ 
x=not 0+0 y=1 ifx!=y thengoto19end num++ 







ifnum!=10then:OUTPUT="Skipped: "+(10-num)+" tests" goto20end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_sqrt() {
        tester(true,
r#"n=1 x=sqrt 24 y=4.899 if x!=y then goto19 end n++ 
x=(sqrt 2) y=1.414 if x!=y then goto19 end n++ 
x=(sqrt 7) y=2.645 if x!=y then goto19 end n++ 
x=(sqrt 32199) y=179.440 if x!=y then goto19 end n++ 
x=(sqrt 1000001) y=1000 if x!=y then goto19 end n++ 
x=(sqrt 1000002) y=1000.001 if x!=y then goto19 end n++ 
x=sqrt 9223372036854775.807 y=-9223372036854775.808 n++ goto19/(x!=y)
x=(sqrt -3) y=-9223372036854775.808 if x!=y then goto19 end n++ 
x=sqrt 9223372036854775 y=-9223372036854775.808 n++ goto19/(x!=y) 
x=sqrt 9223372036854774.999 y=96038388.349 n++ goto19/(x!=y)






if n != 11 then :OUTPUT="Skipped: "+(11-n)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+n+" got: "+x+" but wanted: "+y
goto20"#
        );
    }

    #[test]
    fn acid_string_length() {
        tester(true,
r#"a="字字字字字字字字字字字字字字字字字字字字" //20
b=a b+=b b+=b b+=b b+=b b+=b b+=b b+=b b+=b // b=7680chars
c+=b!="" b-- c+=b!="" b-- c+=b!="" b-- c+=b!="" b-- c+=b!="" b-- goto3
if c==1024 then :OUTPUT="ok" goto4 end
:OUTPUT="Wrong length! got: "+c+" but wanted: 1024" goto5"#
        );
    }

    #[test]
    fn acid_string_length_inv() {
        tester(false,
r#"a="字字字字字字字字字字字字字字字字字字字字" //20
b=a b+=b b+=b b+=b b+=b b+=b b+=b b+=b b+=b // b=7680chars
c+=b!="" b-- c+=b!="" b-- c+=b!="" b-- c+=b!="" b-- c+=b!="" b-- goto3
if c==1023 then :OUTPUT="ok" goto4 end
:OUTPUT="Wrong length! got: "+c+" but wanted: 1024" goto5"#
        );
    }

    #[test]
    fn acid_string_logic() {
        tester(true,
r#"num=1 if "" then goto 19 end num++
if "abc" then goto 19 end num++
if "1" then goto 19 end num++
if "0" then goto 19 end num++
if not "" then goto 19 end num++
if not "1" then goto 19 end num++
if not "0" then goto 19 end num++
if 1 and "" then goto 19 end num++
if 1 and "1" then goto 19 end num++
if 1 and "0" then goto 19 end num++
if not (1 or "") then goto 19 end num++
if not (1 or "1") then goto 19 end num++
if not (1 or "0") then goto 19 end num++
if 0 or "" then goto 19 end num++
if 0 or "1" then goto 19 end num++
if 0 or "0" then goto 19 end num++
if num != 17 then :OUTPUT="Skipped: "+(17-num)+" tests" goto 20 end
:OUTPUT="ok" goto20
:OUTPUT="Failed test #"+num
goto20"#
        );
    }

    #[test]
    fn acid_tan() {
        tester(true,
r#"x=tan(0)
u/=x!=0 :OUTPUT="Failed #1 : " + x goto8
x=tan(45)
u/=x!=1 :OUTPUT="Failed #2 : " + x goto8
x=tan(90)
u/=x!=-22877332.428 :OUTPUT="Failed #3 : " + x goto8
:OUTPUT="ok"
goto8"#
        );
    }
}
