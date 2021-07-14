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
        Expr::Unop(_, _) => todo!(),
        Expr::Binop(l, binop, r) => {
            let arg1 = expression_codegen(vm, data, *r).into_val(vm);
            let arg2 = expression_codegen(vm, data, *l).into_val(vm);
            let out = vm.new_val_reg(Value::default());
            vm.code.push(match binop {
                Binop::Add => Instr::AddV { arg1, arg2, out },
                Binop::Sub => Instr::SubV { arg1, arg2, out },
                Binop::Mul => todo!(),
                Binop::Div => todo!(),
                Binop::Mod => todo!(),
                Binop::Pow => todo!(),
                Binop::Lt => todo!(),
                Binop::Le => todo!(),
                Binop::Eq => todo!(),
                Binop::Ge => todo!(),
                Binop::Gt => todo!(),
                Binop::Ne => todo!(),
                Binop::And => todo!(),
                Binop::Or => todo!(),
            });
            AnyReg::Value(out)
        },
    }
}
