use super::*;

#[derive(Default)]
struct CodegenData {
    var_map: AHashMap<Var, AnyReg>,
}

impl From<Script> for VMExec {
    fn from(script: Script) -> Self {
        let mut vm = VMExec::default();
        let mut data = CodegenData::default();
        for (line_num, line) in IntoIter::new(script.0).enumerate() {
            vm.line_starts[line_num] = vm.code.len();
            vm.code.push(Instr::LineStart(line_num as u8));
            for statement in line.0 {
                match statement {
                    Statement::Assign(var, style, mut expr) =>
                        if let AnyReg::Value(out) = var_codegen(&mut vm, &mut data, var.clone()) {
                            expr = apply_assign_style(var, style, expr);
                            let arg = expression_codegen(&mut vm, &mut data, expr);
                            vm.code.push(match arg {
                                AnyReg::Number(arg) => Instr::MoveNV { arg, out },
                                AnyReg::String(arg) => Instr::MoveSV { arg, out },
                                AnyReg::Value(arg) => Instr::MoveVV { arg, out },
                            })
                        } else {
                            unreachable!();
                        },
                    Statement::IfThenElse(_, _, _) => todo!(),
                    Statement::Goto(expr) => {
                        let num = match expression_codegen(&mut vm, &mut data, expr) {
                            AnyReg::Number(n) => n,
                            AnyReg::String(arg) => {
                                let out = vm.new_val_reg(Value::Str(YString::default()));
                                vm.code.push(Instr::MoveSV { arg, out });
                                let arg = out;
                                let out = vm.new_num_reg(Number::ZERO);
                                vm.code.push(Instr::MoveVN { arg, out });
                                out
                            },
                            AnyReg::Value(arg) => {
                                let out = vm.new_num_reg(Number::ZERO);
                                vm.code.push(Instr::MoveVN { arg, out });
                                out
                            },
                        };
                        vm.code.push(Instr::JumpLine(num));
                    },
                    Statement::PreInc(var) | Statement::PostInc(var) => {
                        expression_codegen(&mut vm, &mut data, Expr::PreInc(var));
                    },
                    Statement::PreDec(var) | Statement::PostDec(var) => {
                        expression_codegen(&mut vm, &mut data, Expr::PreDec(var));
                    },
                }
            }
        }
        vm.globals.extend(data.var_map
            .into_iter()
            .filter(|(var, _)| var.kind == VarKind::Global)
            .map(|(var, reg)| (var.name, reg))
        );
        vm
    }
}

fn apply_assign_style(var: Var, style: AssignStyle, expr: Expr) -> Expr {
    let binop = match style {
        AssignStyle::Normal => return expr,
        AssignStyle::Add => Binop::Add,
        AssignStyle::Sub => Binop::Sub,
        AssignStyle::Mul => Binop::Mul,
        AssignStyle::Div => Binop::Div,
        AssignStyle::Mod => Binop::Mod,
        AssignStyle::Pow => Binop::Pow,
    };
    Expr::Binop(Box::new(Expr::Var(var)), binop, Box::new(expr))
}

fn var_codegen(vm: &mut VMExec, data: &mut CodegenData, var: Var) -> AnyReg {
    data.var_map
        .entry(var)
        .or_insert_with(|| AnyReg::Value(vm.new_val_reg(Value::default())))
        .clone()
}

fn expression_codegen(vm: &mut VMExec, data: &mut CodegenData, expr: Expr) -> AnyReg {
    match expr {
        Expr::NumLit(n) => AnyReg::Number(vm.new_num_reg(n)),
        Expr::StrLit(s) => AnyReg::String(vm.new_string_reg(s)),
        Expr::Var(var) => var_codegen(vm, data, var),
        Expr::PreInc(var) => match var_codegen(vm, data, var) {
            AnyReg::Number(arg) => {
                vm.code.push(Instr::IncN { arg, out: arg });
                AnyReg::Number(arg)
            },
            AnyReg::String(arg) => {
                vm.code.push(Instr::IncS { arg, out: arg });
                AnyReg::String(arg)
            },
            AnyReg::Value(arg) => {
                vm.code.push(Instr::IncV { arg, out: arg });
                AnyReg::Value(arg)
            },
        },
        Expr::PreDec(var) => match var_codegen(vm, data, var) {
            AnyReg::Number(arg) => {
                vm.code.push(Instr::DecN { arg, out: arg });
                AnyReg::Number(arg)
            },
            AnyReg::String(arg) => {
                vm.code.push(Instr::DecS { arg, out: arg });
                AnyReg::String(arg)
            },
            AnyReg::Value(arg) => {
                vm.code.push(Instr::DecV { arg, out: arg });
                AnyReg::Value(arg)
            },
        },
        Expr::PostInc(var) => match var_codegen(vm, data, var) {
            AnyReg::Number(arg) => {
                let out = vm.new_num_reg(Number::ZERO);
                vm.code.push(Instr::IncN { arg, out });
                AnyReg::Number(out)
            },
            AnyReg::String(arg) => {
                let out = vm.new_string_reg(YString(String::with_capacity(STRING_CAP)));
                vm.code.push(Instr::IncS { arg, out });
                AnyReg::String(out)
            },
            AnyReg::Value(arg) => {
                let out = vm.new_val_reg(Value::default());
                vm.code.push(Instr::IncV { arg, out });
                AnyReg::Value(out)
            },
        },
        Expr::PostDec(var) => match var_codegen(vm, data, var) {
            AnyReg::Number(arg) => {
                let out = vm.new_num_reg(Number::ZERO);
                vm.code.push(Instr::DecN { arg, out });
                AnyReg::Number(out)
            },
            AnyReg::String(arg) => {
                let out = vm.new_string_reg(YString(String::with_capacity(STRING_CAP)));
                vm.code.push(Instr::DecS { arg, out });
                AnyReg::String(out)
            },
            AnyReg::Value(arg) => {
                let out = vm.new_val_reg(Value::default());
                vm.code.push(Instr::DecV { arg, out });
                AnyReg::Value(out)
            },
        },
        Expr::Unop(Unop::Not, expr) => {
            let arg = expression_codegen(vm, data, *expr).into_bool(vm);
            let out = vm.new_num_reg(Number::ZERO);
            vm.code.push(Instr::Not { arg, out });
            AnyReg::Number(out)
        },
        Expr::Unop(unop, expr) => {
            let arg = expression_codegen(vm, data, *expr).into_num(vm);
            let out = vm.new_num_reg(Number::ZERO);
            vm.code.push(match unop {
                Unop::Abs => Instr::Abs { arg, out },
                Unop::Sqrt => Instr::Sqrt { arg, out },
                Unop::Sin => Instr::Sin { arg, out },
                Unop::Cos => Instr::Cos { arg, out },
                Unop::Tan => Instr::Tan { arg, out },
                Unop::Asin => Instr::Asin { arg, out },
                Unop::Acos => Instr::Acos { arg, out },
                Unop::Atan => Instr::Atan { arg, out },
                Unop::Fact => Instr::Fact { arg, out },
                Unop::Neg => Instr::Neg { arg, out },
                Unop::Not => unsafe { unreachable() },
            });
            AnyReg::Number(out)
        },
        Expr::Binop(l, binop@(Binop::Add | Binop::Sub), r) => {
            let arg1 = expression_codegen(vm, data, *r).into_val(vm);
            let arg2 = expression_codegen(vm, data, *l).into_val(vm);
            let out = vm.new_val_reg(Value::default());
            match binop {
                Binop::Add => vm.code.push(Instr::AddV { arg1, arg2, out }),
                Binop::Sub => vm.code.push(Instr::SubV { arg1, arg2, out }),
                _ => unsafe { unreachable() },
            }
            AnyReg::Value(out)
        },
        Expr::Binop(
            l,
            binop@(Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Ge | Binop::Gt),
            r,
        ) => {
            let arg1 = expression_codegen(vm, data, *r).into_val(vm);
            let arg2 = expression_codegen(vm, data, *l).into_val(vm);
            let mut out = vm.new_num_reg(Number::ZERO);
            vm.code.push(match binop {
                Binop::Eq | Binop::Ne => Instr::Eq { arg1, arg2, out },
                Binop::Le => Instr::Le { arg1, arg2, out },
                Binop::Lt => Instr::Lt { arg1, arg2, out },
                Binop::Ge => Instr::Le { arg1: arg2, arg2: arg1, out },
                Binop::Gt => Instr::Lt { arg1: arg2, arg2: arg1, out },
                _ => unsafe { unreachable() },
            });
            if binop == Binop::Ne {
                let new_out = vm.new_num_reg(Number::ZERO);
                vm.code.push(Instr::Not { arg: out, out: new_out });
                out = new_out;
            }
            AnyReg::Number(out)
        },
        Expr::Binop(l, binop@(Binop::And | Binop::Or), r) => {
            let arg1 = expression_codegen(vm, data, *r).into_bool(vm);
            let arg2 = expression_codegen(vm, data, *l).into_bool(vm);
            let out = vm.new_num_reg(Number::ZERO);
            vm.code.push(match binop {
                Binop::And => Instr::And { arg1, arg2, out },
                Binop::Or => Instr::Or { arg1, arg2, out },
                _ => unsafe { unreachable() },
            });
            AnyReg::Number(out)
        },
        Expr::Binop(l, binop@(Binop::Mul | Binop::Div | Binop::Mod | Binop::Pow), r) => {
            let arg1 = expression_codegen(vm, data, *r).into_num(vm);
            let arg2 = expression_codegen(vm, data, *l).into_num(vm);
            let out = vm.new_num_reg(Number::ZERO);
            vm.code.push(match binop {
                Binop::Mul => Instr::Mul { arg1, arg2, out },
                Binop::Div => Instr::Div { arg1, arg2, out },
                Binop::Mod => Instr::Mod { arg1, arg2, out },
                Binop::Pow => Instr::Pow { arg1, arg2, out },
                _ => unsafe { unreachable() },
            });
            AnyReg::Number(out)
        },
    }
}

#[cold]
unsafe fn unreachable() -> ! {
    if cfg!(debug_assertions) {
        unreachable!()
    } else {
        std::hint::unreachable_unchecked()
    }
}
