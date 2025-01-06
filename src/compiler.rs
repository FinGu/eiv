use std::{borrow::Borrow, cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{
        Accept, ControlFlowType, ElseStmt, ExprVisitor, FnStmt, LiteralExpr, Statement,
        StmtVisitor, VarExpr,
    },
    lexer::TokenType,
    vm::{Immediate, OpCode},
};

#[derive(Clone, Debug, Default)]
pub struct Function {
    pub name: String,
    pub arity: usize,
    pub code: Vec<OpCode>,
}

#[derive(Clone, Debug)]
pub struct SDefImmediate{
    pub value: Immediate,
    pub is_static: bool
}

impl From<Immediate> for SDefImmediate{
    fn from(value: Immediate) -> Self {
        SDefImmediate{value, is_static: false}
    }
}

#[derive(Clone, Debug, Default)]
pub struct StructDef {
    pub name: String,
    pub data: HashMap<String, SDefImmediate>,
}

impl StructDef {
    pub fn get(&self, name: &str) -> SDefImmediate{
        self.data.get(name).cloned().unwrap_or(Immediate::Null.into())
    }

    pub fn insert(&mut self, name: String, data: SDefImmediate) {
        self.data.insert(name, data);
    }
}

impl Function {
    pub fn get_next(&self, cur: i32) -> OpCode {
        self.code[(cur + 1) as usize].clone()
    }
}

#[derive(Clone, Debug)]
pub struct Local {
    id: String,
    scope: usize,
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    symbols: Vec<Local>,
    depth: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: vec![],
            depth: 0,
        }
    }

    pub fn enter_scope(&mut self) {
        self.depth += 1;
    }

    pub fn leave_scope(&mut self) -> usize {
        let amnt_to_pop = self
            .symbols
            .iter()
            .filter(|el| el.scope >= self.depth)
            .count();

        self.symbols.retain(|el| el.scope < self.depth);

        self.depth -= 1;

        amnt_to_pop
    }

    pub fn resolve(&self, name: &str) -> Option<usize> {
        let len = self.symbols.len();

        for (i, var) in self.symbols.iter().rev().enumerate() {
            //inner to outer
            if var.scope == self.depth && var.id == name {
                return Some(len - i - 1);
            }
        }

        None
    }

    pub fn mark(&mut self, name: String) -> usize {
        let mut out = if !self.symbols.is_empty() {
            self.symbols.len() - 1
        } else {
            0
        };

        let mut should_push = true;

        for (i, var) in self.symbols.iter().enumerate() {
            if var.id != name {
                continue;
            }

            if self.depth <= var.scope {
                should_push = false;
            }

            out = i;
        }

        if should_push {
            self.symbols.push(Local {
                id: name,
                scope: self.depth,
            });

            out = self.symbols.len() - 1;
        }

        out
    }
}

pub enum LoopStmtType {
    Continue,
    Break,
}

#[derive(Default)]
pub struct Loop {
    continuee: i32,
    breakk: i32,
    opcodes: Vec<(usize, LoopStmtType)>, // the position of the Continue and Break opcodes
}

#[derive(Default)]
pub struct LoopContext {
    stmts: Vec<Loop>,
}

impl LoopContext {
    pub fn push_ctx(&mut self) {
        self.stmts.push(Loop::default());
    }

    pub fn pop_ctx(&mut self) -> Loop {
        self.stmts.pop().unwrap()
    }

    pub fn push_pos_to_current(&mut self, pos: usize, kind: LoopStmtType) {
        let last_i = self.stmts.len() - 1;

        let last = &mut self.stmts[last_i];

        last.opcodes.push((pos, kind));
    }

    pub fn set_continue_pos(&mut self, pos: usize) {
        let last_i = self.stmts.len() - 1;

        let last = &mut self.stmts[last_i];

        last.continuee = pos as i32;
    }

    pub fn set_break_pos(&mut self, pos: usize) {
        let last_i = self.stmts.len() - 1;

        let last = &mut self.stmts[last_i];

        last.breakk = pos as i32;
    }
}

pub struct Compiler {
    cur_func: Function,

    pub symbol_table: SymbolTable,

    loop_ctx: LoopContext,

    is_constructor: bool,
}

impl Compiler {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            cur_func: Function::default(),
            symbol_table,
            loop_ctx: LoopContext::default(),
            is_constructor: false,
        }
    }

    pub fn push_cflow(&mut self, kind: LoopStmtType) {
        let location = self.get_cur_stack().len();

        self.loop_ctx.push_pos_to_current(location, kind);

        self.get_cur_stack().push(OpCode::Jump(0));
    }

    pub fn push_return(&mut self) {
        let is_constructor = self.is_constructor;

        let stack = self.get_cur_stack();

        stack.push(if is_constructor {
            OpCode::GetLocal(0)
        } else {
            OpCode::Nop
        });

        stack.push(OpCode::Return);
    }

    pub fn work(&mut self, statements: Vec<Statement>) -> CompilerResult<Function> {
        for stmt in &statements {
            stmt.accept(self)?;
        }

        self.push_return();

        Ok(self.cur_func.clone())
    }

    pub fn get_cur_stack(&mut self) -> &mut Vec<OpCode> {
        &mut self.cur_func.code
    }
}

pub struct StructCompiler<'a> {
    compiler: &'a mut Compiler,
}

impl<'a> StructCompiler<'a> {
    pub fn new(compiler: &'a mut Compiler) -> Self {
        Self { compiler }
    }
}

impl<'a> StmtVisitor for StructCompiler<'a> {
    type Output = CompilerResult<()>;

    fn visit_set_stmt(&mut self, expr: &crate::ast::SetStmt) -> Self::Output {
        expr.accept(self.compiler)
    }
    fn visit_if_stmt(&mut self, expr: &crate::ast::IfStmt) -> Self::Output {
        expr.accept(self.compiler)
    }
    fn visit_for_stmt(&mut self, expr: &crate::ast::ForStmt) -> Self::Output {
        expr.accept(self.compiler)
    }
    fn visit_ctrl_stmt(&mut self, expr: &crate::ast::CtrlStmt) -> Self::Output {
        expr.accept(self.compiler)
    }
    fn visit_block_stmt(&mut self, expr: &crate::ast::BlockStmt) -> Self::Output {
        expr.accept(self.compiler)
    }
    fn visit_while_stmt(&mut self, expr: &crate::ast::WhileStmt) -> Self::Output {
        expr.accept(self.compiler)
    }
    fn visit_struct_stmt(&mut self, expr: &crate::ast::StructStmt) -> Self::Output {
        let name = expr.name.token_type.as_identifier().unwrap();

        let new_struct = RefCell::new(StructDef {
            name: name.clone(),
            ..Default::default()
        });

        let name = expr.name.token_type.as_identifier().unwrap();

        self.compiler.get_cur_stack()
            .push(OpCode::Constant(Immediate::StructDef(new_struct.into())));

        self.compiler.get_cur_stack().push(OpCode::SetStructVar(name.clone()));

        self.compiler.get_cur_stack().push(OpCode::GetStructVar(name.clone()));

        let mut struct_compiler = StructCompiler::new(self.compiler);

        let methods = &expr.methods; //methods isn't accurate

        for method in methods {
            method.accept(&mut struct_compiler)?;
        }

        self.compiler.get_cur_stack().push(OpCode::Pop);

        Ok(())

    }
    fn visit_include_stmt(&mut self, expr: &crate::ast::IncludeStmt) -> Self::Output {
        todo!()
    }

    fn visit_variable_stmt(&mut self, expr: &crate::ast::VarStmt) -> Self::Output {
        let name = expr.name.token_type.as_identifier().unwrap();

        expr.init.accept(self.compiler)?;

        self.compiler
            .get_cur_stack()
            .push(OpCode::SetStructVar(name.clone()));

        Ok(())
    }

    fn visit_function_stmt(&mut self, expr: &FnStmt) -> Self::Output {
        let name = expr.name.token_type.as_identifier().unwrap();

        let params = &expr.params;

        let mut new_sym_table = SymbolTable::new();

        new_sym_table.mark(name.to_string());

        for param in params {
            new_sym_table.mark(param.clone());
        }

        let mut new_compiler = Compiler::new(new_sym_table);

        if name == "constructor" {
            new_compiler.is_constructor = true;
        }

        let mut compiled_result = new_compiler.work(expr.body.clone())?;
        compiled_result.arity = params.len();
        compiled_result.name = name.clone();

        self.compiler
            .get_cur_stack()
            .push(OpCode::Constant(Immediate::Function(
                compiled_result.into(),
            )));

        self.compiler
            .get_cur_stack()
            .push(OpCode::SetStructVar(name.clone()));

        Ok(())
    }

    fn visit_array_set_stmt(&mut self, expr: &crate::ast::ArraySetStmt) -> Self::Output {
        todo!()
    }

    fn visit_expression_stmt(&mut self, expr: &crate::ast::ExprStmt) -> Self::Output {
        expr.accept(self.compiler)
    }

}

impl StmtVisitor for Compiler {
    type Output = CompilerResult<()>;

    fn visit_expression_stmt(&mut self, expr: &crate::ast::ExprStmt) -> Self::Output {
        let our_expr = &expr.expression;

        if our_expr.is_call() {
            return our_expr.accept(self);
        }

        our_expr.accept(self)?;

        self.get_cur_stack().push(OpCode::Pop);

        Ok(())
    }

    fn visit_if_stmt(&mut self, expr: &crate::ast::IfStmt) -> Self::Output {
        expr.cond.accept(self)?;

        let jump_if_false_pos = self.get_cur_stack().len();

        self.get_cur_stack().push(OpCode::JumpIfFalse(0));

        self.get_cur_stack().push(OpCode::Pop);

        expr.then.accept(self)?;

        let jump_after_pos = self.get_cur_stack().len();

        self.get_cur_stack().push(OpCode::Jump(0));

        self.get_cur_stack().push(OpCode::Pop);

        if let OpCode::JumpIfFalse(ref mut imm) = self.get_cur_stack()[jump_if_false_pos] {
            *imm = (jump_after_pos - jump_if_false_pos + 1) as i32;
        }

        if let Some(else_stmt) = &expr.or {
            match else_stmt {
                ElseStmt::Else(el) => el.accept(self)?,
                ElseStmt::ElseIf(elif) => elif.accept(self)?,
            }
        }

        let jump_after_else_pos = self.get_cur_stack().len();

        if let OpCode::Jump(ref mut imm) = self.get_cur_stack()[jump_after_pos] {
            *imm = (jump_after_else_pos - jump_after_pos) as i32;
        }

        Ok(())
    }

    fn visit_set_stmt(&mut self, expr: &crate::ast::SetStmt) -> Self::Output {
        expr.callee.accept(self)?;

        expr.rvalue.accept(self)?;

        let name = expr.name.token_type.as_identifier().unwrap().clone();

        self.get_cur_stack().push(OpCode::SetProp(
            name,
            Box::new(match expr.op.token_type {
                TokenType::PlusEqual => OpCode::Add,
                TokenType::StarEqual => OpCode::Multiply,
                TokenType::MinusEqual => OpCode::Subtract,
                TokenType::SlashEqual => OpCode::Divide,
                //TokenType::PercentageEqual => OpCode::Modulus
                _ => OpCode::Equal,
            }),
        ));

        Ok(())
    }

    fn visit_ctrl_stmt(&mut self, expr: &crate::ast::CtrlStmt) -> Self::Output {
        match &expr.ctrl {
            ControlFlowType::Return(ret) => {
                ret.accept(self)?;
                self.push_return();
            }
            ControlFlowType::Break => {
                self.push_cflow(LoopStmtType::Break);
            }
            ControlFlowType::Continue => {
                self.push_cflow(LoopStmtType::Continue);
            }
            _ => unimplemented!(),
        }

        Ok(())
    }

    fn visit_block_stmt(&mut self, expr: &crate::ast::BlockStmt) -> Self::Output {
        if expr.is_standalone {
            self.symbol_table.enter_scope();
        }

        for stmt in &expr.statements {
            stmt.accept(self)?;
        }

        if expr.is_standalone {
            let amnt = self.symbol_table.leave_scope();
            for _ in 0..amnt {
                self.get_cur_stack().push(OpCode::Pop);
            }
        }

        Ok(())
    }

    fn visit_while_stmt(&mut self, expr: &crate::ast::WhileStmt) -> Self::Output {
        self.loop_ctx.push_ctx();

        let before_cond_pos = self.get_cur_stack().len();

        expr.cond.accept(self)?;

        let jump_if_false_pos = self.get_cur_stack().len();

        self.get_cur_stack().push(OpCode::JumpIfFalse(0));

        self.get_cur_stack().push(OpCode::Pop);

        expr.then.accept(self)?;

        let continue_pos = self.get_cur_stack().len();

        self.loop_ctx.set_continue_pos(continue_pos);

        if let Some(incr) = &expr.incr_stmt {
            incr.accept(self)?;
        }

        let after_jump = self.get_cur_stack().len();

        self.loop_ctx.set_break_pos(after_jump + 1);

        self.get_cur_stack().push(OpCode::Jump(0));

        self.get_cur_stack().push(OpCode::Pop);

        if let OpCode::JumpIfFalse(ref mut imm) = self.get_cur_stack()[jump_if_false_pos] {
            *imm = (after_jump - jump_if_false_pos + 1) as i32;
        }

        if let OpCode::Jump(ref mut imm) = self.get_cur_stack()[after_jump] {
            *imm = before_cond_pos as i32 - after_jump as i32;
        }

        let loop_data = self.loop_ctx.pop_ctx();

        for (pos, kind) in loop_data.opcodes {
            if let OpCode::Jump(ref mut imm) = self.get_cur_stack()[pos] {
                *imm = match kind {
                    LoopStmtType::Continue => loop_data.continuee - pos as i32,
                    LoopStmtType::Break => loop_data.breakk - pos as i32,
                }
            }
        }

        Ok(())
    }

    fn visit_for_stmt(&mut self, expr: &crate::ast::ForStmt) -> Self::Output {
        if let Some(decl) = &expr.decl {
            decl.accept(self)?;
        }

        expr.iwhile.accept(self)
    }

    fn visit_struct_stmt(&mut self, expr: &crate::ast::StructStmt) -> Self::Output {
        let name = expr.name.token_type.as_identifier().unwrap();

        let local = self.symbol_table.mark(name.clone());

        let new_struct = RefCell::new(StructDef {
            name: name.clone(),
            ..Default::default()
        });

        self.get_cur_stack()
            .push(OpCode::Constant(Immediate::StructDef(new_struct.into())));

        self.get_cur_stack().push(OpCode::SetLocal(local as i32));

        self.get_cur_stack().push(OpCode::GetLocal(local as i32));

        let mut struct_compiler = StructCompiler::new(self);

        let methods = &expr.methods; //methods isn't accurate

        for method in methods {
            method.accept(&mut struct_compiler)?;
        }

        self.get_cur_stack().push(OpCode::Pop);

        Ok(())
    }
    fn visit_include_stmt(&mut self, expr: &crate::ast::IncludeStmt) -> Self::Output {
        unimplemented!()
    }

    fn visit_variable_stmt(&mut self, expr: &crate::ast::VarStmt) -> Self::Output {
        let name = expr.name.token_type.as_identifier().unwrap();

        expr.init.accept(self)?;

        let pos = self.symbol_table.mark(name.clone());

        self.get_cur_stack().push(OpCode::SetLocal(pos as i32));

        Ok(())
    }

    fn visit_function_stmt(&mut self, expr: &crate::ast::FnStmt) -> Self::Output {
        let name = expr.name.token_type.as_identifier().unwrap();

        let params = &expr.params;

        let mut new_sym_table = SymbolTable::new();

        new_sym_table.mark(name.to_string());

        for param in params {
            new_sym_table.mark(param.clone());
        }

        let mut new_compiler = Compiler::new(new_sym_table);

        let mut compiled_result = new_compiler.work(expr.body.clone())?;
        compiled_result.arity = params.len();
        compiled_result.name = name.clone();

        let local = self.symbol_table.mark(name.clone());

        self.get_cur_stack()
            .push(OpCode::Constant(Immediate::Function(
                compiled_result.into(),
            )));

        self.get_cur_stack().push(OpCode::SetLocal(local as i32));

        Ok(())
    }

    fn visit_array_set_stmt(&mut self, expr: &crate::ast::ArraySetStmt) -> Self::Output {
        unimplemented!()
    }
}

impl ExprVisitor for Compiler {
    type Output = CompilerResult<()>;

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Output {
        let data = &expr.value;

        self.get_cur_stack().push(OpCode::Constant(data.clone()));

        Ok(())
    }

    fn visit_get_expr(&mut self, expr: &crate::ast::GetExpr) -> Self::Output {
        expr.callee.accept(self)?;

        let name = expr.argument.token_type.as_identifier().unwrap().clone();

        self.get_cur_stack().push(OpCode::GetProp(name));

        Ok(())
    }

    fn visit_cast_expr(&mut self, expr: &crate::ast::CastExpr) -> Self::Output {
        expr.argument.accept(self)?;

        self.get_cur_stack().push(match expr.op.token_type {
            TokenType::BoolCast => OpCode::BoolCast,
            TokenType::CharCast => OpCode::CharCast,
            TokenType::NumberCast => OpCode::NumberCast,
            _ => unreachable!(),
        });
        Ok(())
    }

    fn visit_call_expr(&mut self, expr: &crate::ast::CallExpr) -> Self::Output {
        let mut arg_amount = 0;

        expr.callee.accept(self)?;

        for arg in &expr.arguments {
            arg.accept(self)?;

            arg_amount += 1;
        }

        self.get_cur_stack().push(OpCode::Call(arg_amount));

        Ok(())
    }

    fn visit_this_expr(&mut self, _: &crate::ast::ThisExpr) -> Self::Output {
        self.get_cur_stack().push(OpCode::This);
        Ok(())
    }

    fn visit_array_expr(&mut self, expr: &crate::ast::ArrayExpr) -> Self::Output {
        unimplemented!()
    }

    fn visit_unary_expr(&mut self, expr: &crate::ast::UnaryExpr) -> Self::Output {
        expr.right.accept(self)?;

        self.get_cur_stack().push(match expr.op.token_type {
            TokenType::Minus => OpCode::Negate,
            TokenType::Not => OpCode::Not,
            _ => unreachable!(),
        });

        Ok(())
    }

    fn visit_binary_expr(&mut self, expr: &crate::ast::BinaryExpr) -> Self::Output {
        expr.left.accept(self)?;
        expr.right.accept(self)?;

        self.get_cur_stack().push(match expr.op.token_type {
            TokenType::Plus => OpCode::Add,
            TokenType::Minus => OpCode::Subtract,
            TokenType::Star => OpCode::Multiply,
            TokenType::Slash => OpCode::Divide,
            TokenType::EqualEqual => OpCode::Equal,
            TokenType::NotEqual => OpCode::NotEqual,
            TokenType::Greater => OpCode::Greater,
            TokenType::GreaterEqual => OpCode::GreaterEqual,
            TokenType::Less => OpCode::Less,
            TokenType::LessEqual => OpCode::LessEqual,
            TokenType::Or => OpCode::Or,
            TokenType::And => OpCode::And,
            _ => {
                unreachable!()
            }
        });

        Ok(())
    }

    fn visit_grouping_expr(&mut self, expr: &crate::ast::GroupingExpr) -> Self::Output {
        expr.expr.accept(self)?;
        Ok(())
    }

    fn visit_variable_expr(&mut self, expr: &crate::ast::VarExpr) -> Self::Output {
        let name = expr.name.token_type.as_identifier().unwrap().clone();

        let local = self.symbol_table.resolve(&name);

        if let Some(resolved) = local {
            self.get_cur_stack().push(OpCode::GetLocal(resolved as i32));
            return Ok(());
        }

        self.get_cur_stack().push(OpCode::GetGlobal(name));

        Ok(())
    }

    fn visit_array_get_expr(&mut self, expr: &crate::ast::ArrayGetExpr) -> Self::Output {
        unimplemented!()
    }
}
#[derive(thiserror::Error, Debug)]
pub enum CompilerError {
    #[error("Generic error")]
    Generic,
    #[error("Unresolved symbol")]
    UnresolvedSymbol,
}

pub type CompilerResult<T> = Result<T, CompilerError>;
