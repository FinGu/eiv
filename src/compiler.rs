use crate::{ast::{Accept, ControlFlowType, ElseStmt, ExprVisitor, LiteralExpr, Statement, StmtVisitor}, lexer::TokenType, vm::{ Immediate, OpCode}};

#[derive(Clone, Debug)]
pub struct Function{
    pub name: String,
    pub arity: usize,
    pub code: Vec<OpCode>,
}

impl Function{
    pub fn new() -> Self{
        Self{
            name: String::new(),
            arity: 0,
            code: Vec::new()
        }
    }

    pub fn get_next(&self, cur: i32) -> OpCode{
        self.code[(cur+1) as usize].clone()
    }
}

#[derive(Clone, Debug)]
pub struct Local{
    id: String,
    scope: usize,
}
//a = 5
//println(a)
//Constant(5) SetLocal(0) GetLocal(0) GetGobal("println") Call
//a = 5
//{
//a = 7
//}
//
//a = 5
//b = 6
//{ 
//a = 7
//}

//a, b, a
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

    pub fn leave_scope(&mut self) -> usize{
        let amnt_to_pop = self.symbols.iter().filter(|el| el.scope >= self.depth).count();

        self.symbols.retain(|el| el.scope < self.depth);

        self.depth -= 1;

        amnt_to_pop
    }

    pub fn resolve(&self, name: &str) -> Option<usize> {
        let len = self.symbols.len();

        for (i, var) in self.symbols.iter().rev().enumerate() { //inner to outer
            if var.scope == self.depth && var.id == name{
                return Some(len - i - 1); 
            } 
        }

        None
    }

    //Local{ a, 0 }
    //

    pub fn mark(&mut self, name: String) -> usize {
        let mut out = if !self.symbols.is_empty(){
            self.symbols.len()-1 
        } else{
            0
        };

        let mut should_push = true;

        for (i, var) in self.symbols.iter().rev().enumerate(){
            if var.id != name{
                continue;
            }

            if self.depth <= var.scope{
                should_push = false;
            } 

            out = i;
        }

        if should_push{
            self.symbols.push(Local{
                id: name,
                scope: self.depth
            });

            out = self.symbols.len()-1;
        }

        out
    }
}

pub struct Compiler{
    cur_func: Function,

    pub symbol_table: SymbolTable,
}

impl Compiler{
    pub fn new(symbol_table: SymbolTable) -> Self{
        Self{
            cur_func: Function::new(),
            symbol_table, 
        }
    }

    pub fn push_return(&mut self){
        let stack = self.get_cur_stack();

        stack.push(OpCode::Nop);
        stack.push(OpCode::Return);
    }

    pub fn work(&mut self, statements: Vec<Statement>) -> CompilerResult<Function>{
        for stmt in &statements{
            stmt.accept(self)?;
        }

        self.push_return();

        Ok(self.cur_func.clone())
    }

    pub fn get_cur_stack(&mut self) -> &mut Vec<OpCode>{
        &mut self.cur_func.code
    }
}

impl StmtVisitor for Compiler{
    type Output = CompilerResult<()>;

    fn visit_expression_stmt(&mut self, expr: &crate::ast::ExprStmt) -> Self::Output {
        let our_expr = &expr.expression;

        if our_expr.is_call(){
            return our_expr.accept(self);
        }

        our_expr.accept(self)?;

        self.get_cur_stack().push(OpCode::Pop);

        Ok(())
    }

    //comp
    //jump_caso_falso
    //then
    //jump saida
    //else
    //saida
    fn visit_if_stmt(&mut self, expr: &crate::ast::IfStmt) -> Self::Output {
        expr.cond.accept(self)?;

        let jump_if_false_pos = self.get_cur_stack().len();

        self.get_cur_stack().push(OpCode::JumpIfFalse(0));

        self.get_cur_stack().push(OpCode::Pop);

        expr.then.accept(self)?;

        let jump_after_pos = self.get_cur_stack().len();

        self.get_cur_stack().push(OpCode::Jump(0));

        self.get_cur_stack().push(OpCode::Pop);

        if let OpCode::JumpIfFalse(ref mut imm) = self.get_cur_stack()[jump_if_false_pos]{
            *imm = (jump_after_pos - jump_if_false_pos + 1) as i32;
        }
        
        if let Some(else_stmt) = &expr.or {
            match else_stmt{
                ElseStmt::Else(el) => el.accept(self)?,
                ElseStmt::ElseIf(elif) => elif.accept(self)?
            }
        }

        let jump_after_else_pos = self.get_cur_stack().len();

        if let OpCode::Jump(ref mut imm) = self.get_cur_stack()[jump_after_pos]{
            *imm = (jump_after_else_pos - jump_after_pos + 1) as i32;
        }

        Ok(())
    }

    fn visit_set_stmt(&mut self, expr: &crate::ast::SetStmt) -> Self::Output {
        unimplemented!()
    }
    fn visit_ctrl_stmt(&mut self, expr: &crate::ast::CtrlStmt) -> Self::Output {
        match &expr.ctrl{
            ControlFlowType::Return(ret) => {
                ret.accept(self)?;
                self.push_return();
            },
            _ => unimplemented!()
        }

        Ok(())
    }
    fn visit_block_stmt(&mut self, expr: &crate::ast::BlockStmt) -> Self::Output {
        if expr.is_standalone{
            self.symbol_table.enter_scope();
        }

        for stmt in &expr.statements{
            stmt.accept(self)?;
        }

        if expr.is_standalone{
            let amnt = self.symbol_table.leave_scope();
            for _ in 0..amnt{
                self.get_cur_stack().push(OpCode::Pop);
            } 
        }

        Ok(())
    }

    //cond
    //j_nao_bater saida
    //then
    //incremento
    //jmp acima_cond
    //saida

    fn visit_while_stmt(&mut self, expr: &crate::ast::WhileStmt) -> Self::Output {
        let before_cond_pos = self.get_cur_stack().len();

        expr.cond.accept(self)?;

        let jump_if_false_pos = self.get_cur_stack().len();

        self.get_cur_stack().push(OpCode::JumpIfFalse(0));

        self.get_cur_stack().push(OpCode::Pop);

        expr.then.accept(self)?;

        if let Some(incr) = &expr.incr_stmt{
            incr.accept(self)?;
        }

        let after_jump = self.get_cur_stack().len();

        self.get_cur_stack().push(OpCode::Jump(0));

        self.get_cur_stack().push(OpCode::Pop);

        if let OpCode::JumpIfFalse(ref mut imm) = self.get_cur_stack()[jump_if_false_pos]{
            *imm = (after_jump - jump_if_false_pos + 1) as i32;
        }
        
        if let OpCode::Jump(ref mut imm) = self.get_cur_stack()[after_jump] {
            *imm = before_cond_pos as i32 - after_jump as i32 - 1;
        }

        Ok(())
    }

    fn visit_for_stmt(&mut self, expr: &crate::ast::ForStmt) -> Self::Output {
        if let Some(decl) = &expr.decl{
            decl.accept(self)?;
        }

        expr.iwhile.accept(self)
    }


    fn visit_struct_stmt(&mut self, expr: &crate::ast::StructStmt) -> Self::Output {
        unimplemented!()
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

        for param in params{
            new_sym_table.mark(param.clone());
        }

        let mut new_compiler = Compiler::new(new_sym_table);

        let mut compiled_result = new_compiler.work(expr.body.clone())?;
        compiled_result.arity = params.len();

        let local = self.symbol_table.mark(name.clone());

        self.get_cur_stack().push(OpCode::Constant(Immediate::Function(compiled_result)));

        self.get_cur_stack().push(OpCode::StartFn(local as i32));

        Ok(())
    }

    fn visit_array_set_stmt(&mut self, expr: &crate::ast::ArraySetStmt) -> Self::Output {
        unimplemented!()
    }
}

impl ExprVisitor for Compiler{
    type Output = CompilerResult<()>;

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Output {
        let data = &expr.value;

        self.get_cur_stack().push(OpCode::Constant(data.clone()));

        Ok(())
    }

    fn visit_get_expr(&mut self, expr: &crate::ast::GetExpr) -> Self::Output {
        unimplemented!()
    }

    fn visit_cast_expr(&mut self, expr: &crate::ast::CastExpr) -> Self::Output {
        expr.argument.accept(self)?;

        self.get_cur_stack().push(match expr.op.token_type{
            TokenType::BoolCast => OpCode::BoolCast,
            TokenType::CharCast => OpCode::CharCast,
            TokenType::NumberCast => OpCode::NumberCast,
            _ => unreachable!()
        });
        Ok(())
    }

    fn visit_call_expr(&mut self, expr: &crate::ast::CallExpr) -> Self::Output {
        let mut arg_amount = 0;

        for arg in &expr.arguments{
            arg.accept(self)?;

            arg_amount += 1;
        }

        expr.callee.accept(self)?;

        self.get_cur_stack().push(OpCode::Call(arg_amount));

        Ok(())
    }

    fn visit_this_expr(&mut self, expr: &crate::ast::ThisExpr) -> Self::Output {
        unimplemented!()
    }
    fn visit_array_expr(&mut self, expr: &crate::ast::ArrayExpr) -> Self::Output {
        unimplemented!()
    }
    fn visit_unary_expr(&mut self, expr: &crate::ast::UnaryExpr) -> Self::Output {
        expr.right.accept(self)?;

        self.get_cur_stack().push(match expr.op.token_type{
            TokenType::Minus => OpCode::Negate,
            TokenType::Not => OpCode::Not,
            _ => unreachable!()
        });

        Ok(())
    }
    fn visit_binary_expr(&mut self, expr: &crate::ast::BinaryExpr) -> Self::Output {
        expr.left.accept(self)?;
        expr.right.accept(self)?;

        self.get_cur_stack().push(match expr.op.token_type{
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

        if let Some(resolved) = local{
            self.get_cur_stack().push(OpCode::GetLocal(resolved as i32));
            return Ok(());
        }

        self.get_cur_stack().push(OpCode::GetGlobal(name));

        Ok(())
        //Err(CompilerError::UnresolvedSymbol)
    }

    fn visit_array_get_expr(&mut self, expr: &crate::ast::ArrayGetExpr) -> Self::Output {
        unimplemented!()
    }
}
#[derive(thiserror::Error, Debug)]
pub enum CompilerError{
    #[error("Generic error")]
    Generic,
    #[error("Unresolved symbol")]
    UnresolvedSymbol,
}

pub type CompilerResult<T> = Result<T, CompilerError>;
