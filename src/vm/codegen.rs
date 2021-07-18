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
            vm.code.push(HLInstr::LineStart(line_num as u8));
            for statement in line.0 {
                vm.codegen_statement(statement, &mut data);
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
    fn codegen_statement(&mut self, statement: Statement, data: &mut CodegenData) {
        match statement {
            Statement::Assign(var, style, mut expr) =>
                if let AnyReg::Value(out) = var_codegen(self, data, var.clone()) {
                    expr = apply_assign_style(var, style, expr);
                    let arg = expression_codegen(self, data, expr);
                    self.code.push(match arg {
                        AnyReg::Number(arg) => HLInstr::MoveNV { arg, out },
                        AnyReg::String(arg) => HLInstr::MoveSV { arg, out },
                        AnyReg::Value(arg) => HLInstr::MoveVV { arg, out },
                    })
                } else {
                    unreachable!();
                },
            Statement::IfThenElse(cond, t, e) => {
                let cond = expression_codegen(self, data, cond).into_num(self);
                let cond_len = self.code.len();
                self.code.push(HLInstr::JumpRel { amount: 0, condition: None });
                for statement in e {
                    self.codegen_statement(statement, data);
                }
                let e_len = self.code.len();
                self.code.push(HLInstr::JumpRel { amount: 0, condition: None });
                for statement in t {
                    self.codegen_statement(statement, data);
                }
                let t_len = self.code.len();
                self.code[cond_len] = HLInstr::JumpRel {
                    amount: e_len - cond_len + 1,
                    condition: Some(cond),
                };
                self.code[e_len] = HLInstr::JumpRel {
                    amount: t_len - e_len + 1,
                    condition: None,
                };
            }
            Statement::Goto(expr) => {
                let num = match expression_codegen(self, data, expr) {
                    AnyReg::Number(n) => n,
                    AnyReg::String(arg) => {
                        let out = self.new_val_reg(Value::Str(YString::default()));
                        self.code.push(HLInstr::MoveSV { arg, out });
                        let arg = out;
                        let out = self.new_num_reg(Number::ZERO);
                        self.code.push(HLInstr::MoveVN { arg, out });
                        self.code.push(HLInstr::JumpErr);
                        out
                    },
                    AnyReg::Value(arg) => {
                        let out = self.new_num_reg(Number::ZERO);
                        self.code.push(HLInstr::MoveVN { arg, out });
                        self.code.push(HLInstr::JumpErr);
                        out
                    },
                };
                self.code.push(HLInstr::JumpLine(num));
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
                vm.code.push(HLInstr::IncN { arg, out: arg });
                AnyReg::Number(arg)
            },
            AnyReg::String(arg) => {
                vm.code.push(HLInstr::IncS { arg, out: arg });
                AnyReg::String(arg)
            },
            AnyReg::Value(arg) => {
                vm.code.push(HLInstr::IncV { arg, out: arg });
                AnyReg::Value(arg)
            },
        },
        Expr::PreDec(var) => match var_codegen(vm, data, var) {
            AnyReg::Number(arg) => {
                vm.code.push(HLInstr::DecN { arg, out: arg });
                AnyReg::Number(arg)
            },
            AnyReg::String(arg) => {
                vm.code.push(HLInstr::DecS { arg, out: arg });
                AnyReg::String(arg)
            },
            AnyReg::Value(arg) => {
                vm.code.push(HLInstr::DecV { arg, out: arg });
                AnyReg::Value(arg)
            },
        },
        Expr::PostInc(var) => match var_codegen(vm, data, var) {
            AnyReg::Number(arg) => {
                let out = vm.new_num_reg(Number::ZERO);
                vm.code.push(HLInstr::IncN { arg, out });
                AnyReg::Number(out)
            },
            AnyReg::String(arg) => {
                let out = vm.new_string_reg(YString(String::with_capacity(STRING_CAP)));
                vm.code.push(HLInstr::IncS { arg, out });
                AnyReg::String(out)
            },
            AnyReg::Value(arg) => {
                let out = vm.new_val_reg(Value::default());
                vm.code.push(HLInstr::IncV { arg, out });
                AnyReg::Value(out)
            },
        },
        Expr::PostDec(var) => match var_codegen(vm, data, var) {
            AnyReg::Number(arg) => {
                let out = vm.new_num_reg(Number::ZERO);
                vm.code.push(HLInstr::DecN { arg, out });
                AnyReg::Number(out)
            },
            AnyReg::String(arg) => {
                let out = vm.new_string_reg(YString(String::with_capacity(STRING_CAP)));
                vm.code.push(HLInstr::DecS { arg, out });
                AnyReg::String(out)
            },
            AnyReg::Value(arg) => {
                let out = vm.new_val_reg(Value::default());
                vm.code.push(HLInstr::DecV { arg, out });
                AnyReg::Value(out)
            },
        },
        Expr::Unop(Unop::Not, expr) => {
            let arg = expression_codegen(vm, data, *expr).into_bool(vm);
            let out = vm.new_num_reg(Number::ZERO);
            vm.code.push(HLInstr::Not { arg, out });
            AnyReg::Number(out)
        },
        Expr::Unop(unop, expr) => {
            let arg = expression_codegen(vm, data, *expr).into_num(vm);
            let out = vm.new_num_reg(Number::ZERO);
            vm.code.push(match unop {
                Unop::Abs => HLInstr::Abs { arg, out },
                Unop::Sqrt => HLInstr::Sqrt { arg, out },
                Unop::Sin => HLInstr::Sin { arg, out },
                Unop::Cos => HLInstr::Cos { arg, out },
                Unop::Tan => HLInstr::Tan { arg, out },
                Unop::Asin => HLInstr::Asin { arg, out },
                Unop::Acos => HLInstr::Acos { arg, out },
                Unop::Atan => HLInstr::Atan { arg, out },
                Unop::Fact => HLInstr::Fact { arg, out },
                Unop::Neg => HLInstr::Neg { arg, out },
                Unop::Not => unsafe { unreachable() },
            });
            AnyReg::Number(out)
        },
        Expr::Binop(l, binop@(Binop::Add | Binop::Sub), r) => {
            let arg1 = expression_codegen(vm, data, *r).into_val(vm);
            let arg2 = expression_codegen(vm, data, *l).into_val(vm);
            let out = vm.new_val_reg(Value::default());
            match binop {
                Binop::Add => vm.code.push(HLInstr::AddV { arg1, arg2, out }),
                Binop::Sub => vm.code.push(HLInstr::SubV { arg1, arg2, out }),
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
                Binop::Eq | Binop::Ne => HLInstr::Eq { arg1, arg2, out },
                Binop::Le => HLInstr::Le { arg1, arg2, out },
                Binop::Lt => HLInstr::Lt { arg1, arg2, out },
                Binop::Ge => HLInstr::Le { arg1: arg2, arg2: arg1, out },
                Binop::Gt => HLInstr::Lt { arg1: arg2, arg2: arg1, out },
                _ => unsafe { unreachable() },
            });
            if binop == Binop::Ne {
                let new_out = vm.new_num_reg(Number::ZERO);
                vm.code.push(HLInstr::Not { arg: out, out: new_out });
                out = new_out;
            }
            AnyReg::Number(out)
        },
        Expr::Binop(l, binop@(Binop::And | Binop::Or), r) => {
            let arg1 = expression_codegen(vm, data, *r).into_bool(vm);
            let arg2 = expression_codegen(vm, data, *l).into_bool(vm);
            let out = vm.new_num_reg(Number::ZERO);
            vm.code.push(match binop {
                Binop::And => HLInstr::And { arg1, arg2, out },
                Binop::Or => HLInstr::Or { arg1, arg2, out },
                _ => unsafe { unreachable() },
            });
            AnyReg::Number(out)
        },
        Expr::Binop(l, binop@(Binop::Mul | Binop::Div | Binop::Mod | Binop::Pow), r) => {
            let arg1 = expression_codegen(vm, data, *r).into_num(vm);
            let arg2 = expression_codegen(vm, data, *l).into_num(vm);
            let out = vm.new_num_reg(Number::ZERO);
            vm.code.push(match binop {
                Binop::Mul => HLInstr::Mul { arg1, arg2, out },
                Binop::Div => HLInstr::Div { arg1, arg2, out },
                Binop::Mod => HLInstr::Mod { arg1, arg2, out },
                Binop::Pow => HLInstr::Pow { arg1, arg2, out },
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
