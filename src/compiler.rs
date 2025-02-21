use std::{cell::RefCell, collections::HashMap, fs};

use serde::{Deserialize, Serialize};

use crate::{
    ast::{
        Accept, ControlFlowType, ElseStmt, ExprVisitor, FnStmt, LiteralExpr, Statement, StmtVisitor,
    },
    errors,
    lexer::{Lexer, TokenType},
    parser::Parser,
    vm::{DeclOpCode, Immediate, OpCode},
};

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub arity: usize,
    pub code: Vec<OpCode>,
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct StructDef {
    pub name: String,
    pub normal_data: HashMap<String, Immediate>,
    pub static_data: HashMap<String, Immediate>,
}

impl StructDef {
    pub fn get_normal(&self, name: &str) -> Immediate {
        self.normal_data
            .get(name)
            .cloned()
            .unwrap_or(Immediate::Null)
    }

    pub fn insert_normal(&mut self, name: String, data: Immediate) {
        self.normal_data.insert(name, data);
    }

    pub fn get_static(&self, name: &str) -> Immediate {
        self.static_data
            .get(name)
            .cloned()
            .unwrap_or(Immediate::Null)
    }

    pub fn insert_static(&mut self, name: String, data: Immediate) {
        self.static_data.insert(name, data);
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
    cur_struct_name: String,

    cur_func: Function,

    pub symbol_table: SymbolTable,

    loop_ctx: LoopContext,

    is_constructor: bool,
    repl_mode: bool,
}

impl Compiler {
    pub fn new(symbol_table: SymbolTable, repl_mode: bool) -> Self {
        Self {
            cur_struct_name: String::new(),
            cur_func: Function::default(),
            symbol_table,
            loop_ctx: LoopContext::default(),
            is_constructor: false,
            repl_mode,
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

        stack.extend([
            if is_constructor {
                OpCode::GetLocal(0)
            } else {
                OpCode::Nop
            },
            OpCode::Return,
        ]);
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

    fn must_be_global(&self) -> bool {
        self.symbol_table.depth == 0 && self.repl_mode
    }
}

pub struct StructCompiler<'a> {
    name: &'a str,
    compiler: &'a mut Compiler,
}

impl<'a> StructCompiler<'a> {
    pub fn new(name: &'a str, compiler: &'a mut Compiler) -> Self {
        Self { name, compiler }
    }
}

impl<'a> StmtVisitor for StructCompiler<'a> {
    type Output = CompilerResult<()>;

    fn visit_set_stmt(&mut self, expr: &crate::ast::SetStmt) -> Self::Output {
        expr.accept(self.compiler)
    }

    fn visit_if_stmt(&mut self, _: &crate::ast::IfStmt) -> Self::Output {
        unreachable!()
    }

    fn visit_for_stmt(&mut self, _: &crate::ast::ForStmt) -> Self::Output {
        unreachable!()
    }

    fn visit_ctrl_stmt(&mut self, _: &crate::ast::CtrlStmt) -> Self::Output {
        unreachable!()
    }

    fn visit_block_stmt(&mut self, _: &crate::ast::BlockStmt) -> Self::Output {
        unreachable!()
    }

    fn visit_while_stmt(&mut self, _: &crate::ast::WhileStmt) -> Self::Output {
        unreachable!()
    }

    fn visit_struct_stmt(&mut self, expr: &crate::ast::StructStmt) -> Self::Output {
        let name = expr.name.token_type.as_identifier().unwrap();

        let new_struct = RefCell::new(StructDef {
            name: name.clone(),
            ..Default::default()
        });

        self.compiler.symbol_table.mark(name.clone());

        self.compiler
            .get_cur_stack()
            .push(OpCode::Constant(Immediate::StructDef(new_struct.into())));

        if expr.is_static {
            self.compiler.get_cur_stack().extend([
                OpCode::SetStaticStructVar(name.clone()),
                OpCode::GetStaticStructVar(name.clone()),
            ]);
        } else {
            self.compiler.get_cur_stack().extend([
                OpCode::SetStructVar(name.clone()),
                OpCode::GetStructVar(name.clone()),
            ]);
        }

        let mut struct_compiler = StructCompiler::new(name, self.compiler);

        let methods = &expr.methods; //methods isn't accurate

        for method in methods {
            method.accept(&mut struct_compiler)?;
        }

        self.compiler.get_cur_stack().push(OpCode::Pop);

        Ok(())
    }

    fn visit_include_stmt(&mut self, expr: &crate::ast::IncludeStmt) -> Self::Output {
        expr.accept(self.compiler)
    }

    fn visit_variable_stmt(&mut self, expr: &crate::ast::VarStmt) -> Self::Output {
        let name = expr.name.token_type.as_identifier().unwrap();

        expr.init.accept(self.compiler)?;

        self.compiler.get_cur_stack().push(if expr.is_static {
            OpCode::SetStaticStructVar(name.clone())
        } else {
            OpCode::SetStructVar(name.clone())
        });

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

        let mut new_compiler = Compiler::new(new_sym_table, self.compiler.repl_mode);
        new_compiler.cur_struct_name = self.name.to_string();

        //this is a trick in order to recognize if someone's trying to construct an instance of a
        //struct inside itself, such as:
        //a = { fun = (){ return a() } }
        //a_inst.fun() SHOULD return a new instance of a

        if name == "constructor" {
            new_compiler.is_constructor = true;
        }

        let mut compiled_result = new_compiler.work(expr.body.clone())?;
        compiled_result.arity = params.len();
        compiled_result.name = name.clone();

        self.compiler.get_cur_stack().extend([
            OpCode::Constant(compiled_result.into()),
            if expr.is_static {
                OpCode::SetStaticStructVar(name.clone())
            } else {
                OpCode::SetStructVar(name.clone())
            },
        ]);

        Ok(())
    }

    fn visit_array_set_stmt(&mut self, expr: &crate::ast::ArraySetStmt) -> Self::Output {
        expr.accept(self.compiler)
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

        if !self.repl_mode {
            // in repl mode we want the last stack value to be the printed
            self.get_cur_stack().push(OpCode::Pop);
        }

        Ok(())
    }

    fn visit_if_stmt(&mut self, expr: &crate::ast::IfStmt) -> Self::Output {
        expr.cond.accept(self)?;

        let jump_if_false_pos = self.get_cur_stack().len();

        self.get_cur_stack()
            .extend([OpCode::JumpIfFalse(0), OpCode::Pop]);

        expr.then.accept(self)?;

        let jump_after_pos = self.get_cur_stack().len();

        self.get_cur_stack().extend([OpCode::Jump(0), OpCode::Pop]);

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
            *imm = (jump_after_else_pos - jump_after_pos) as i32 - 1;
        }

        Ok(())
    }

    fn visit_set_stmt(&mut self, expr: &crate::ast::SetStmt) -> Self::Output {
        expr.callee.accept(self)?;

        expr.rvalue.accept(self)?;

        let name = expr.name.token_type.as_identifier().unwrap().clone();

        self.get_cur_stack().push(OpCode::SetProp(
            name,
            match expr.op.token_type {
                TokenType::PlusEqual => DeclOpCode::Add,
                TokenType::StarEqual => DeclOpCode::Multiply,
                TokenType::MinusEqual => DeclOpCode::Subtract,
                TokenType::SlashEqual => DeclOpCode::Divide,
                //TokenType::PercentageEqual => OpCode::Modulus
                _ => DeclOpCode::Equal,
            },
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

        self.get_cur_stack()
            .extend([OpCode::JumpIfFalse(0), OpCode::Pop]);

        expr.then.accept(self)?;

        let continue_pos = self.get_cur_stack().len();

        self.loop_ctx.set_continue_pos(continue_pos);

        if let Some(incr) = &expr.incr_stmt {
            incr.accept(self)?;
        }

        let after_jump = self.get_cur_stack().len();

        self.loop_ctx.set_break_pos(after_jump + 1);

        self.get_cur_stack().extend([OpCode::Jump(0), OpCode::Pop]);

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

        let new_struct = RefCell::new(StructDef {
            name: name.clone(),
            ..Default::default()
        });

        self.get_cur_stack()
            .push(OpCode::Constant(Immediate::StructDef(new_struct.into())));

        if self.must_be_global() {
            self.get_cur_stack().extend([
                OpCode::SetGlobal(name.clone()),
                OpCode::GetGlobal(name.clone()),
            ]);
        } else {
            let local = self.symbol_table.mark(name.clone());

            self.get_cur_stack().extend([
                OpCode::SetLocal(local as i32),
                OpCode::GetLocal(local as i32),
            ]);
        }

        let mut struct_compiler = StructCompiler::new(name, self);

        let methods = &expr.methods; //methods isn't accurate

        for method in methods {
            method.accept(&mut struct_compiler)?;
        }

        self.get_cur_stack().push(OpCode::Pop);

        Ok(())
    }

    fn visit_include_stmt(&mut self, expr: &crate::ast::IncludeStmt) -> Self::Output {
        let name = &expr.file;

        let inner = match fs::read_to_string(name) {
            Ok(var) => var,
            Err(_) => {
                return Err(CompilerError::InvalidFile);
            }
        };

        let mut scanner = Lexer::new(inner);

        let tokens = scanner.work();

        errors::LIST.lock().unwrap().report();

        let mut parser = Parser::new(tokens, name);

        let statements = parser.work();

        errors::LIST.lock().unwrap().report();

        let old_symbol_table = self.symbol_table.clone();

        let mut comp = Compiler::new(old_symbol_table, self.repl_mode);

        let result = comp.work(statements).unwrap();

        let cur_stack = self.get_cur_stack();

        let code = result.code;

        let len = code.len();

        cur_stack.extend(code.into_iter().take(len - 2));

        self.symbol_table = comp.symbol_table;
        //the idea behind is compile it as function, and remove the two last arguments, that are
        //nop and return

        Ok(())
    }

    fn visit_variable_stmt(&mut self, expr: &crate::ast::VarStmt) -> Self::Output {
        let name = expr.name.token_type.as_identifier().unwrap();

        expr.init.accept(self)?;

        if self.must_be_global() {
            self.get_cur_stack()
                .push(OpCode::SetGlobal(name.to_string()));
        } else {
            let pos = self.symbol_table.mark(name.clone());

            self.get_cur_stack().push(OpCode::SetLocal(pos as i32));
        }

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

        let mut new_compiler = Compiler::new(new_sym_table, self.repl_mode);

        let mut compiled_result = new_compiler.work(expr.body.clone())?;
        compiled_result.arity = params.len();
        compiled_result.name = name.clone();

        let set_opcode = if self.must_be_global() {
            OpCode::SetGlobal(name.clone())
        } else {
            let local = self.symbol_table.mark(name.clone());
            OpCode::SetLocal(local as i32)
        };

        self.get_cur_stack()
            .extend([OpCode::Constant(compiled_result.into()), set_opcode]);

        Ok(())
    }

    fn visit_array_set_stmt(&mut self, expr: &crate::ast::ArraySetStmt) -> Self::Output {
        expr.callee.accept(self)?;

        expr.argument.accept(self)?;

        expr.rvalue.accept(self)?;

        self.get_cur_stack().push(OpCode::SetArrayIndex);

        Ok(())
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

        if expr.is_static {
            self.get_cur_stack().push(OpCode::GetStaticProp(name));
        } else {
            self.get_cur_stack().push(OpCode::GetProp(name));
        }

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
        let exprs = &expr.exprs;

        let mut arg_amount = 0;

        for expr in exprs {
            expr.accept(self)?;
            arg_amount += 1;
        }

        let size = expr
            .size
            .as_ref()
            .and_then(|s| s.token_type.as_number())
            .cloned()
            .unwrap_or(0.0) as usize;

        self.get_cur_stack()
            .push(OpCode::CaptureArray(arg_amount, size));

        Ok(())
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

        if name == self.cur_struct_name {
            self.get_cur_stack().push(OpCode::ConstructThis);
            return Ok(());
        }

        let local = self.symbol_table.resolve(&name);

        if let Some(resolved) = local {
            self.get_cur_stack().push(OpCode::GetLocal(resolved as i32));
            return Ok(());
        }

        self.get_cur_stack().push(OpCode::GetGlobal(name));

        Ok(())
    }

    fn visit_array_get_expr(&mut self, expr: &crate::ast::ArrayGetExpr) -> Self::Output {
        expr.callee.accept(self)?;

        expr.argument.accept(self)?;

        self.get_cur_stack().push(OpCode::GetArrayIndex);

        Ok(())
    }
}
#[derive(thiserror::Error, Debug)]
pub enum CompilerError {
    #[error("Not a valid file to include")]
    InvalidFile,
}

pub type CompilerResult<T> = Result<T, CompilerError>;
