use super::*;

#[derive(Default)]
struct CodegenData {
    var_map: AHashMap<Var, AnyReg>,
}

impl From<Script> for VMExec {
    fn from(script: Script) -> Self {
        let mut vm = VMExec::default();
        let mut data = CodegenData::default();
        for (line_num, mut line) in IntoIter::new(script.0).enumerate() {
            vm.line_starts[line_num] = vm.code.len();
            vm.code.push(Instr::line_start(line_num as u8));
            if let Some(last) = line.0.pop() {
                for statement in line.0 {
                    vm.codegen_statement(statement, &mut data, false);
                }
                vm.codegen_statement(last, &mut data, true);
                vm.code.last_mut().unwrap().set_line_end(true);
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

impl VMExec {
    fn codegen_statement(&mut self, statement: Statement, data: &mut CodegenData, last: bool) {
        match statement {
            Statement::Assign(var, style, mut expr) =>
                if let AnyReg::Value(out) = var_codegen(self, data, var.clone()) {
                    expr = apply_assign_style(var, style, expr);
                    let arg = expression_codegen(self, data, expr);
                    self.code.push(match arg {
                        AnyReg::Number(arg) => Instr::move_nv(arg, out),
                        AnyReg::String(arg) => Instr::move_sv(arg, out),
                        AnyReg::Value(arg) => Instr::move_vv(arg, out),
                    })
                } else {
                    unreachable!();
                },
            Statement::IfThenElse(cond, t, e) => {
                let cond = expression_codegen(self, data, cond).into_bool(self);
                let cond_len = self.code.len();
                self.code.push(Instr::jump_rel(0, None));
                for statement in e {
                    self.codegen_statement(statement, data, false);
                }
                let e_len = self.code.len();
                self.code.push(Instr::jump_rel(0, None));
                for statement in t {
                    self.codegen_statement(statement, data, false);
                }
                let t_len = self.code.len();
                self.code[cond_len] = Instr::jump_rel(e_len - cond_len, Some(cond));
                self.code[e_len] = Instr::jump_rel(t_len - e_len, None);
                self.code[e_len].set_line_end(last);
            }
            Statement::Goto(expr) => {
                let num = match expression_codegen(self, data, expr) {
                    AnyReg::Number(n) => n,
                    AnyReg::String(arg) => {
                        let out = self.new_val_reg(Value::Str(YString::default()));
                        self.code.push(Instr::move_sv(arg, out));
                        let arg = out;
                        let out = self.new_num_reg(Number::ZERO);
                        self.code.push(Instr::move_vn(arg, out));
                        self.code.push(Instr::jump_err());
                        out
                    },
                    AnyReg::Value(arg) => {
                        let out = self.new_num_reg(Number::ZERO);
                        self.code.push(Instr::move_vn(arg, out));
                        self.code.push(Instr::jump_err());
                        out
                    },
                };
                self.code.push(Instr::jump_line(num));
            },
            Statement::PreInc(var) | Statement::PostInc(var) => {
                expression_codegen(self, data, Expr::PreInc(var));
            },
            Statement::PreDec(var) | Statement::PostDec(var) => {
                expression_codegen(self, data, Expr::PreDec(var));
            },
        }
    }
}

fn apply_assign_style(var: Var, style: AssignStyle, expr: Expr) -> Expr {
    let binop = match style {
        AssignStyle::Normal => return expr,
        AssignStyle::Add => Binop::Add,
        AssignStyle::Sub => Binop::Sub,
        AssignStyle::Mul => Binop::Mul,
        AssignStyle::Div => Binop::Div,
        AssignStyle::Rem => Binop::Rem,
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
                vm.code.push(Instr::inc_n(arg, arg));
                AnyReg::Number(arg)
            },
            AnyReg::String(arg) => {
                vm.code.push(Instr::inc_s(arg, arg));
                AnyReg::String(arg)
            },
            AnyReg::Value(arg) => {
                vm.code.push(Instr::inc_v(arg, arg));
                AnyReg::Value(arg)
            },
        },
        Expr::PreDec(var) => match var_codegen(vm, data, var) {
            AnyReg::Number(arg) => {
                vm.code.push(Instr::dec_n(arg, arg));
                AnyReg::Number(arg)
            },
            AnyReg::String(arg) => {
                vm.code.push(Instr::dec_s(arg, arg));
                AnyReg::String(arg)
            },
            AnyReg::Value(arg) => {
                vm.code.push(Instr::dec_v(arg, arg));
                AnyReg::Value(arg)
            },
        },
        Expr::PostInc(var) => match var_codegen(vm, data, var) {
            AnyReg::Number(arg) => {
                let out = vm.new_num_reg(Number::ZERO);
                vm.code.push(Instr::inc_n(arg, out));
                AnyReg::Number(out)
            },
            AnyReg::String(arg) => {
                let out = vm.new_string_reg(YString(String::with_capacity(STRING_CAP)));
                vm.code.push(Instr::inc_s(arg, out));
                AnyReg::String(out)
            },
            AnyReg::Value(arg) => {
                let out = vm.new_val_reg(Value::default());
                vm.code.push(Instr::inc_v(arg, out));
                AnyReg::Value(out)
            },
        },
        Expr::PostDec(var) => match var_codegen(vm, data, var) {
            AnyReg::Number(arg) => {
                let out = vm.new_num_reg(Number::ZERO);
                vm.code.push(Instr::dec_n(arg, out));
                AnyReg::Number(out)
            },
            AnyReg::String(arg) => {
                let out = vm.new_string_reg(YString(String::with_capacity(STRING_CAP)));
                vm.code.push(Instr::dec_s(arg, out));
                AnyReg::String(out)
            },
            AnyReg::Value(arg) => {
                let out = vm.new_val_reg(Value::default());
                vm.code.push(Instr::dec_v(arg, out));
                AnyReg::Value(out)
            },
        },
        Expr::Unop(Unop::Not, expr) => {
            let arg = expression_codegen(vm, data, *expr).into_val(vm);
            let out = vm.new_num_reg(Number::ZERO);
            vm.code.push(Instr::not_v(arg, out));
            AnyReg::Number(out)
        },
        Expr::Unop(unop, expr) => {
            let arg = expression_codegen(vm, data, *expr).into_num(vm);
            let out = vm.new_num_reg(Number::ZERO);
            vm.code.push(match unop {
                Unop::Abs => Instr::abs(arg, out),
                Unop::Sqrt => Instr::sqrt(arg, out),
                Unop::Sin => Instr::sin(arg, out),
                Unop::Cos => Instr::cos(arg, out),
                Unop::Tan => Instr::tan(arg, out),
                Unop::Asin => Instr::asin(arg, out),
                Unop::Acos => Instr::acos(arg, out),
                Unop::Atan => Instr::atan(arg, out),
                Unop::Fact => Instr::fact(arg, out),
                Unop::Neg => Instr::neg(arg, out),
                Unop::Not => unsafe { unreachable() },
            });
            AnyReg::Number(out)
        },
        Expr::Binop(l, binop@(Binop::Add | Binop::Sub), r) => {
            let arg2 = expression_codegen(vm, data, *r).into_val(vm);
            let arg1 = expression_codegen(vm, data, *l).into_val(vm);
            let out = vm.new_val_reg(Value::default());
            match binop {
                Binop::Add => vm.code.push(Instr::add_v(arg1, arg2, out)),
                Binop::Sub => vm.code.push(Instr::sub_v(arg1, arg2, out)),
                _ => unsafe { unreachable() },
            }
            AnyReg::Value(out)
        },
        Expr::Binop(
            l,
            binop@(Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Ge | Binop::Gt),
            r,
        ) => {
            let arg2 = expression_codegen(vm, data, *r).into_val(vm);
            let arg1 = expression_codegen(vm, data, *l).into_val(vm);
            let mut out = vm.new_num_reg(Number::ZERO);
            vm.code.push(match binop {
                Binop::Eq | Binop::Ne => Instr::eq(arg1, arg2, out),
                Binop::Le => Instr::le(arg1, arg2, out),
                Binop::Lt => Instr::lt(arg1, arg2, out),
                Binop::Ge => Instr::le(arg2, arg1, out),
                Binop::Gt => Instr::lt(arg2, arg1, out),
                _ => unsafe { unreachable() },
            });
            if binop == Binop::Ne {
                let new_out = vm.new_num_reg(Number::ZERO);
                vm.code.push(Instr::not_n(out, new_out));
                out = new_out;
            }
            AnyReg::Number(out)
        },
        Expr::Binop(l, binop@(Binop::And | Binop::Or), r) => {
            let arg2 = expression_codegen(vm, data, *r).into_bool(vm);
            let arg1 = expression_codegen(vm, data, *l).into_bool(vm);
            let out = vm.new_num_reg(Number::ZERO);
            vm.code.push(match binop {
                Binop::And => Instr::and(arg1, arg2, out),
                Binop::Or => Instr::or(arg1, arg2, out),
                _ => unsafe { unreachable() },
            });
            AnyReg::Number(out)
        },
        Expr::Binop(l, binop@(Binop::Mul | Binop::Div | Binop::Rem | Binop::Pow), r) => {
            let arg2 = expression_codegen(vm, data, *r).into_num(vm);
            let arg1 = expression_codegen(vm, data, *l).into_num(vm);
            let out = vm.new_num_reg(Number::ZERO);
            vm.code.push(match binop {
                Binop::Mul => Instr::mul(arg1, arg2, out),
                Binop::Div => Instr::div(arg1, arg2, out),
                Binop::Rem => Instr::rem(arg1, arg2, out),
                Binop::Pow => Instr::pow(arg1, arg2, out),
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
