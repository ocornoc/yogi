use parser::*;
use super::*;

#[derive(Debug, Clone)]
pub struct CodegenOptions {
    pub protect_locals: bool,
    pub protect_globals: bool,
}

impl Default for CodegenOptions {
    fn default() -> Self {
        Self {
            protect_locals: false,
            protect_globals: true,
        }
    }
}

struct CodegenData {
    sections: Vec<SectionCode>,
    lines: Vec<Section>,
    current_line: usize,
    numbers: Vec<Number>,
    strings: Vec<YString>,
    values: Vec<Value>,
    idents: AHashMap<Ident, ValReg>,
    pub options: CodegenOptions,
}

impl CodegenData {
    fn next_line(&self) -> usize {
        if self.current_line == self.lines.len() - 1 {
            0
        } else {
            self.current_line + 1
        }
    }

    fn add_jmperr(&mut self, section: Section) {
        let line = self.next_line();
        self.sections[section.0].instrs.push(Instruction::JumpIfError(self.lines[line]));
    }

    fn make_truthy(&mut self, section: Section, r: ValReg) -> NumReg {
        let n = self.numbers.len().into();
        self.numbers.push(0.into());
        self.sections[section.0].instrs.push(Instruction::IsTruthyVal(r, n));
        n
    }

    fn numberify(&mut self, section: Section, r: ValReg) -> NumReg {
        let n = self.numbers.len().into();
        self.numbers.push(0.into());
        self.sections[section.0].instrs.push(Instruction::NumberifyVal(r, n));
        self.add_jmperr(section);
        n
    }

    fn make_val(&mut self, section: Section, r: AnyReg) -> ValReg {
        match r {
            AnyReg::Num(n) => {
                let v = self.values.len().into();
                self.values.push(Default::default());
                self.sections[section.0].instrs.push(Instruction::ValueifyNum(n, v));
                v
            },
            AnyReg::Str(s) => {
                let v = self.values.len().into();
                self.values.push(Default::default());
                self.sections[section.0].instrs.push(Instruction::ValueifyStr(s, v));
                v
            },
            AnyReg::Val(v) => v,
        }
    }

    fn make_arith_binop(&mut self, section: Section, l: ValReg, op: Binop, r: ValReg) -> ValReg {
        let r = self.numberify(section, r);
        let l = self.numberify(section, l);
        let instr = match op {
            Binop::Mul => Instruction::Mul(l, r),
            Binop::Div => Instruction::Div(l, r),
            Binop::Mod => Instruction::Rem(l, r),
            Binop::Pow => Instruction::Pow(l, r),
            _ => unreachable!()
        };
        self.sections[section.0].instrs.push(instr);
        if instr.can_runtime_err() {
            self.add_jmperr(section);
        }
        self.make_val(section, l.into())
    }

    fn make_cmp_binop(&mut self, section: Section, l: ValReg, op: Binop, r: ValReg) -> ValReg {
        let n = self.numbers.len().into();
        self.numbers.push(0.into());
        self.sections[section.0].instrs.push(match op {
            Binop::Eq | Binop::Ne => Instruction::Eq(l, r, n),
            Binop::Le => Instruction::Le(l, r, n),
            Binop::Lt => Instruction::Lt(l, r, n),
            Binop::Ge => Instruction::Le(r, l, n),
            Binop::Gt => Instruction::Lt(r, l, n),
            _ => unreachable!(),
        });
        if op == Binop::Ne {
            self.sections[section.0].instrs.push(Instruction::NotNum(n));
        }
        self.make_val(section, n.into())
    }

    fn copy_valreg(&mut self, section: Section, r: ValReg) -> ValReg {
        let v = self.values.len().into();
        self.values.push(Default::default());
        self.sections[section.0].instrs.push(Instruction::CopyVal(r, v));
        v
    }

    fn codegen_from_binop(&mut self, section: Section, l: Expr, op: Binop, r: Expr) -> ValReg {
        let mut r = self.codegen_from_expr(section, r);
        let mut l = self.codegen_from_expr(section, l);
        r = self.copy_valreg(section, r);
        l = self.copy_valreg(section, l);
        match op {
            Binop::And => {
                let r = self.make_truthy(section, r);
                let l = self.make_truthy(section, l);
                self.sections[section.0].instrs.push(Instruction::And(l, r));
                self.make_val(section, l.into())
            },
            Binop::Or => {
                let r = self.make_truthy(section, r);
                let l = self.make_truthy(section, l);
                self.sections[section.0].instrs.push(Instruction::Or(l, r));
                self.make_val(section, l.into())
            },
            Binop::Add => {
                self.sections[section.0].instrs.push(Instruction::AddVal(l, r));
                l
            },
            Binop::Sub => {
                self.sections[section.0].instrs.push(Instruction::SubVal(l, r));
                l
            },
            Binop::Mul | Binop::Div | Binop::Mod | Binop::Pow =>
                self.make_arith_binop(section, l, op, r),
            Binop::Eq | Binop::Ne | Binop::Le | Binop::Lt | Binop::Ge | Binop::Gt =>
                self.make_cmp_binop(section, l, op, r),
        }
    }

    fn codegen_from_unop(&mut self, section: Section, op: Unop, r: Expr) -> ValReg {
        let mut r = self.codegen_from_expr(section, r);
        r = self.copy_valreg(section, r);
        if op == Unop::Not {
            let n = self.numbers.len().into();
            self.numbers.push(0.into());
            self.sections[section.0].instrs.push(Instruction::NotVal(r, n));
            return self.make_val(section, n.into());
        }
        let n = self.numberify(section, r);
        let instr = match op {
            Unop::Not => unreachable!(),
            Unop::Neg => Instruction::Neg(n),
            Unop::Abs => Instruction::Abs(n),
            Unop::Sqrt => Instruction::Sqrt(n),
            Unop::Fact => Instruction::Fact(n),
            Unop::Sin => Instruction::Sin(n),
            Unop::Cos => Instruction::Cos(n),
            Unop::Tan => Instruction::Tan(n),
            Unop::Asin => Instruction::Asin(n),
            Unop::Acos => Instruction::Acos(n),
            Unop::Atan => Instruction::Atan(n),
        };
        self.sections[section.0].instrs.push(instr);
        self.make_val(section, n.into())
    }

    fn get_variable(&mut self, ident: Ident) -> ValReg {
        self.idents.entry(ident).or_insert_with(|| {
            let v = ValReg(self.values.len());
            self.values.push(Default::default());
            v
        }).clone()
    }

    fn codegen_incdec(&mut self, section: Section, incdec: Incdec) -> ValReg {
        let var = self.get_variable(incdec.ident);
        self.sections[section.0].instrs.push(if incdec.inc {
            Instruction::IncVal(var)
        } else {
            Instruction::DecVal(var)
        });
        self.add_jmperr(section);
        var
    }

    fn codegen_from_expr(&mut self, section: Section, expr: Expr) -> ValReg {
        match expr {
            Expr::Binop(l, op, r) => self.codegen_from_binop(section, *l, op, *r),
            Expr::Unop(op, r) => self.codegen_from_unop(section, op, *r),
            Expr::Incdec(incdec) => self.codegen_incdec(section, incdec),
            Expr::Ident(x) => self.get_variable(x),
            Expr::Number(n) => {
                let nreg = NumReg(self.numbers.len());
                self.numbers.push(n);
                self.make_val(section, nreg.into())
            },
            Expr::String(s) => {
                let sreg = StrReg(self.strings.len());
                self.strings.push(s);
                self.make_val(section, sreg.into())
            },
        }
    }

    fn new_section(&mut self, line_start: bool) -> Section {
        let section = Section(self.sections.len());
        self.sections.push(SectionCode {
            instrs: Vec::with_capacity(6),
            line_start,
            success: SUCCESS_NEEDS_FIXING,
        });
        section
    }

    fn codegen_from_ite(
        &mut self,
        section: Section,
        c: Expr,
        t: Vec<Statement>,
        e: Vec<Statement>,
    ) -> Section {
        let c = self.codegen_from_expr(section, c);
        let c = self.make_truthy(section, c);
        let then_link = self.codegen_and_link_stmts(false, t);
        let then_end = if let Some((then_start, then_end)) = then_link {
            self.sections[section.0].instrs.push(Instruction::JumpSectionIf(then_start, c));
            then_end
        } else {
            self.new_section(false).into()
        };
        let else_link = self.codegen_and_link_stmts(false, e);
        let else_end = if let Some((else_start, else_end)) = else_link {
            self.sections[section.0].success = else_start.into();
            else_end
        } else {
            let new_sect = self.new_section(false);
            self.sections[section.0].success = new_sect.into();
            new_sect.into()
        };
        let section = self.new_section(false);
        if let Some(then_end) = then_end {
            self.sections[then_end.0].success = section.into();
        }
        if let Some(else_end) = else_end {
            self.sections[else_end.0].success = section.into();
        }
        section
    }

    fn codegen_from_assign(&mut self, section: Section, x: Ident, op: Option<AssignOp>, e: Expr) {
        let e = self.codegen_from_expr(section, e);
        let var = self.get_variable(x);
        let instr = match op {
            Some(AssignOp::Add) => Instruction::AddVal(var, e),
            Some(AssignOp::Sub) => Instruction::SubVal(var, e),
            Some(op@(AssignOp::Mul | AssignOp::Div | AssignOp::Mod | AssignOp::Pow)) => {
                let out = self.make_arith_binop(section, var, op.into(), e);
                Instruction::CopyVal(out, var)
            },
            None => Instruction::CopyVal(e, var),
        };
        self.sections[section.0].instrs.push(instr);
    }

    fn codegen_from_stmt(
        &mut self,
        stmt: Statement,
        line_start: bool,
    ) -> (Section, Option<Section>) {
        let section = self.new_section(line_start);
        (section, match stmt {
            Statement::Goto(e) => {
                let line = self.codegen_from_expr(section, e);
                let line = self.numberify(section, line);
                self.sections[section.0].success = line.into();
                None
            },
            Statement::Ite(c, t, e) => self.codegen_from_ite(section, c, t, e).into(),
            Statement::Incdec(incdec) => {
                self.codegen_incdec(section, incdec);
                Some(section)
            },
            Statement::Assign(x, op, e) => {
                self.codegen_from_assign(section, x, op, e);
                Some(section)
            },
        })
    }

    fn codegen_and_link_stmts(
        &mut self,
        mut line_start: bool,
        stmts: impl IntoIterator<Item = Statement>,
    ) -> Option<(Section, Option<Section>)> {
        stmts
            .into_iter()
            .map(|stmt| {
                let root_end = self.codegen_from_stmt(stmt, line_start);
                line_start = false;
                root_end
            })
            .collect::<Vec<_>>()
            .into_iter()
            .reduce(|first, next| if let Some(end) = first.1 {
                debug_assert_eq!(self.sections[end.0].success, SUCCESS_NEEDS_FIXING);
                self.sections[end.0].success = SectionOrLine::Section(next.0);
                (first.0, next.1)
            } else {
                first
            })
    }

    fn codegen_from_line(&mut self, line: Line) -> Option<(Section, Option<Section>)> {
        self.codegen_and_link_stmts(true, line.stmts)
    }

    fn codegen_from_program(&mut self, program: Program) {
        for line in program.lines.into_iter() {
            if let Some((start, end)) = self.codegen_from_line(line) {
                if let Some(end) = end {
                    debug_assert_eq!(self.sections[end.0].success, SUCCESS_NEEDS_FIXING);
                    self.sections[end.0].success = self.lines[self.next_line()].into();
                }
                self.sections.swap(self.lines[self.current_line].0, start.0);
            }

            if self.sections[self.current_line].success == SUCCESS_NEEDS_FIXING {
                self.sections[self.current_line].success = self.lines[self.next_line()].into();
            }
            
            self.current_line += 1;
        }
    }
}

impl Default for CodegenData {
    fn default() -> Self {
        CodegenData {
            sections: vec![],
            lines: vec![],
            current_line: 0,
            numbers: Vec::with_capacity(100),
            strings: Vec::with_capacity(100),
            values: Vec::with_capacity(100),
            idents: AHashMap::with_capacity(100),
            options: Default::default(),
        }
    }
}

impl IRMachine {
    pub fn from_ast(options: CodegenOptions, program: parser::Program) -> Self {
        let mut codegen = CodegenData {
            sections: vec![SectionCode {
                instrs: Vec::new(),
                line_start: true,
                success: SUCCESS_NEEDS_FIXING,
            }; program.len()],
            lines: (0..program.len()).map(Section).collect::<Vec<_>>().try_into().unwrap(),
            options,
            ..Default::default()
        };
        codegen.codegen_from_program(program);
        IRMachine {
            sections: codegen.sections,
            current_sect: codegen.lines[0],
            lines: codegen.lines,
            runtime_err: false.into(),
            numbers: codegen.numbers.into_iter().map(AtomicRefCell::new).collect(),
            strings: codegen.strings.into_iter().map(AtomicRefCell::new).collect(),
            values: codegen.values.into_iter().map(AtomicRefCell::new).collect(),
            idents: codegen.idents
                .into_iter()
                .filter(|(i, _)| if i.global {
                    codegen.options.protect_globals
                } else {
                    codegen.options.protect_locals
                })
                .map(|(k, v)| (k, v.into()))
                .collect(),
        }
    }
}