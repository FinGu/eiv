
use crate::{ast::{Accept, CallExpr, ElseStmt, ExprVisitor, LiteralExpr, Statement, StmtVisitor}, lexer::TokenType, vm::{Immediate, OpCode}};
pub struct Compiler{
    output: Vec<OpCode>,
    scope: usize
}

impl Compiler{
    pub fn new() -> Self{
        Self{
            output: Vec::new(),
            scope: 0,
        }
    }

    pub fn work(&mut self, statements: Vec<Statement>) -> Vec<OpCode>{
        for stmt in &statements{
            let _ = stmt.accept(self);
        }

        self.output.clone()
    }
}

impl StmtVisitor for Compiler{
    type Output = CompilerResult<()>;

    fn visit_expression_stmt(&mut self, expr: &crate::ast::ExprStmt) -> Self::Output {
        expr.expression.accept(self)?;
        self.output.push(OpCode::Pop);
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

        let jump_if_false_pos = self.output.len();

        self.output.push(OpCode::JumpIfFalse(0));

        self.output.push(OpCode::Pop);

        expr.then.accept(self)?;

        let jump_after_pos = self.output.len();

        self.output.push(OpCode::Jump(0));

        self.output.push(OpCode::Pop);

        if let OpCode::JumpIfFalse(ref mut imm) = self.output[jump_if_false_pos]{
            *imm = jump_after_pos - jump_if_false_pos + 1;
        }
        
        if let Some(else_stmt) = &expr.or {
            match else_stmt{
                ElseStmt::Else(el) => el.accept(self)?,
                ElseStmt::ElseIf(elif) => elif.accept(self)?
            }
        }

        let jump_after_else_pos = self.output.len();

        if let OpCode::Jump(ref mut imm) = self.output[jump_after_pos]{
            *imm = jump_after_else_pos - jump_after_pos + 1;
        }

        Ok(())
    }

    fn visit_for_stmt(&mut self, expr: &crate::ast::ForStmt) -> Self::Output {
        unimplemented!()
    }

    fn visit_set_stmt(&mut self, expr: &crate::ast::SetStmt) -> Self::Output {
        unimplemented!()
    }
    fn visit_ctrl_stmt(&mut self, expr: &crate::ast::CtrlStmt) -> Self::Output {
        unimplemented!()
    }
    fn visit_block_stmt(&mut self, expr: &crate::ast::BlockStmt) -> Self::Output {
        self.scope += 1;

        for stmt in &expr.statements{
            stmt.accept(self)?;
        }

        self.scope -= 1;

        Ok(())
    }
    fn visit_while_stmt(&mut self, expr: &crate::ast::WhileStmt) -> Self::Output {
        unimplemented!()
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

        self.output.push(OpCode::SetLocal(name.to_string(), self.scope));

        Ok(())
    }

    fn visit_function_stmt(&mut self, expr: &crate::ast::FnStmt) -> Self::Output {
        unimplemented!()
    }
    fn visit_array_set_stmt(&mut self, expr: &crate::ast::ArraySetStmt) -> Self::Output {
        unimplemented!()
    }
}

impl ExprVisitor for Compiler{
    type Output = CompilerResult<()>;

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Output {
        let data = &expr.value;

        self.output.push(OpCode::Constant(data.clone()));

        Ok(())
    }

    fn visit_get_expr(&mut self, expr: &crate::ast::GetExpr) -> Self::Output {
        unimplemented!()
    }
    fn visit_cast_expr(&mut self, expr: &crate::ast::CastExpr) -> Self::Output {
        unimplemented!()
    }
    fn visit_call_expr(&mut self, expr: &crate::ast::CallExpr) -> Self::Output {
        expr.arguments[0].accept(self)?;

        self.output.push(OpCode::TmpPrint);

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

        self.output.push(match expr.op.token_type{
            TokenType::Minus => OpCode::Negate,
            TokenType::Not => OpCode::Not,
            _ => unreachable!()
        });

        Ok(())
    }
    fn visit_binary_expr(&mut self, expr: &crate::ast::BinaryExpr) -> Self::Output {
        expr.left.accept(self)?;
        expr.right.accept(self)?;

        self.output.push(match expr.op.token_type{
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

        self.output.push(OpCode::GetLocal(name, self.scope));

        Ok(())
    }

    fn visit_array_get_expr(&mut self, expr: &crate::ast::ArrayGetExpr) -> Self::Output {
        unimplemented!()
    }
}
#[derive(thiserror::Error, Debug)]
pub enum CompilerError{
    #[error("Generic error")]
    Generic
}

pub type CompilerResult<T> = Result<T, CompilerError>;
