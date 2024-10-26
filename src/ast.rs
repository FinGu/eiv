pub mod funcs;
pub mod structs;
pub mod ustructs;

use std::{fs, sync::Mutex};

use funcs::{Clock, Print, PrintLn, TypeOf, UserFunction, _TypeOf};
use structs::{
    Accept, ArrayExpr, ArrayGetExpr, ArraySetStmt, AstError, BinaryExpr, BlockStmt, CallExpr, Callable, CastExpr, ControlFlowType, CtrlStmt, ElseStmt, ExprStmt, ExprVisitor, Expression, FnStmt, ForStmt, GetExpr, GroupingExpr, IfStmt, IncludeStmt, LiteralExpr, SetStmt, Statement, StmtVisitor, StructStmt, ThisExpr, UnaryExpr, Value, VarExpr, VarStmt, WhileStmt
};
use ustructs::{UserStructDef, UserStructInst};

use crate::{
    errors, lexer::TokenType, utils, vars::{self, VarMap, VARMAP}
};

impl<Visitor, T> Accept<Visitor> for LiteralExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_literal_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for BinaryExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_binary_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for CastExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_cast_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for GroupingExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_grouping_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for ArrayExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;
    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_array_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for UnaryExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_unary_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for VarExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_variable_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for ArrayGetExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_array_get_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for CallExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_call_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for GetExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_get_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for ThisExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_this_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for Expression
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        match *self {
            Expression::Unary(ref unary) => unary.accept(visitor),
            Expression::Binary(ref binary) => binary.accept(visitor),
            Expression::Literal(ref literal) => literal.accept(visitor),
            Expression::Array(ref arr) => arr.accept(visitor),
            Expression::Cast(ref cast) => cast.accept(visitor),
            Expression::Grouping(ref group) => group.accept(visitor),
            Expression::Variable(ref var) => var.accept(visitor),
            Expression::ArrayGet(ref arr) => arr.accept(visitor),
            Expression::Call(ref call) => call.accept(visitor),
            Expression::Get(ref get) => get.accept(visitor),
            Expression::This(ref this) => this.accept(visitor),
        }
    }
}

impl<Visitor, T> Accept<Visitor> for Statement
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        match *self {
            Statement::Expression(ref expr) => expr.accept(visitor),
            Statement::Variable(ref var) => var.accept(visitor),
            Statement::Function(ref fun) => fun.accept(visitor),
            Statement::Block(ref block) => block.accept(visitor),
            Statement::If(ref iff) => iff.accept(visitor),
            Statement::While(ref wwhile) => wwhile.accept(visitor),
            Statement::For(ref ffor) => ffor.accept(visitor),
            Statement::ControlFlow(ref cflow) => cflow.accept(visitor),
            Statement::Include(ref inc) => inc.accept(visitor),
            Statement::Struct(ref sstruct) => sstruct.accept(visitor),
            Statement::Set(ref set) => set.accept(visitor),
            Statement::ArraySet(ref arrset) => arrset.accept(visitor),
            Statement::Static(_) => panic!("This should never be visited"),
        }
    }
}

impl<Visitor, T> Accept<Visitor> for ExprStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_expression_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for VarStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_variable_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for FnStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_function_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for StructStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_struct_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for BlockStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_block_stmt(self)
    }
}
impl<Visitor, T> Accept<Visitor> for IfStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_if_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for WhileStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_while_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for ForStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_for_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for CtrlStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_ctrl_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for IncludeStmt 
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_include_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for SetStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_set_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for ArraySetStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &Visitor) -> Self::Output {
        visitor.visit_array_set_stmt(self)
    }
}

pub fn __execute_block(stmt_eval: &StmtEvaluator, statements: &Vec<Statement>) -> ControlFlowType {
    for stat in statements {
        let result = stat.accept(stmt_eval);

        match result {
            // in case it's a break or continue we have to propagate upwards
            ControlFlowType::None => {}
            out => return out,
        }
    }

    ControlFlowType::None
}

pub struct StmtEvaluator;

impl StmtVisitor for StmtEvaluator {
    type Output = ControlFlowType;

    fn visit_expression_stmt(&self, expr: &ExprStmt) -> Self::Output {
        let eval = ExprEvaluator;

        let _ = expr.expression.accept(&eval);

        ControlFlowType::None
    }

    fn visit_variable_stmt(&self, expr: &VarStmt) -> Self::Output {
        let eval = ExprEvaluator;

        let name = expr
            .name
            .token_type
            .as_identifier()
            .expect("Unexpected lexing of the name of the variable");

        let expr_value = expr.init.accept(&eval);

        let as_copy: Value = match expr_value {
            Value::Array(arr) => arr.lock().unwrap().clone().into(),
            Value::StructInst(inst) => {
                let env = (*inst.env.lock().unwrap()).clone();

                UserStructInst::new(env).into()
            }
            _ => expr_value,
        };

        VARMAP.lock().unwrap().insert(name.clone(), as_copy);

        ControlFlowType::None
    }

    fn visit_function_stmt(&self, expr: &FnStmt) -> Self::Output {
        let name = expr
            .name
            .token_type
            .as_identifier()
            .expect("Unexpected lexing of the name of the function");

        VARMAP.lock().unwrap().insert(
            name.clone(),
            UserFunction::new(expr.params.clone(), expr.body.clone()).into(),
        );

        ControlFlowType::None
    }

    fn visit_struct_stmt(&self, expr: &StructStmt) -> Self::Output {
        let name = expr
            .name
            .token_type
            .as_identifier()
            .expect("Unexpected lexing of the name of the function");

        let strct = UserStructDef::new(self, &expr.methods);

        VARMAP.lock().unwrap().insert(name.clone(), strct.into());

        ControlFlowType::None
    }

    fn visit_block_stmt(&self, expr: &BlockStmt) -> Self::Output {
        if expr.is_standalone {
            vars::create_inner(false);
        }

        let result = __execute_block(self, &expr.statements);

        if expr.is_standalone {
            vars::delete_inner();
        }

        result
    }

    fn visit_if_stmt(&self, expr: &IfStmt) -> Self::Output {
        let eval = ExprEvaluator;

        let main_expr = expr.cond.accept(&eval);

        let result = match main_expr {
            Value::Boolean(b) => b,
            _ => {
                errors::LIST
                    .lock()
                    .unwrap()
                    .push(AstError::BadConditionalExpression, None);
                return ControlFlowType::None;
            }
        };

        if result {
            expr.then.accept(self)
        } else if let Some(ref els) = expr.or {
            match *els {
                // we don't need another visit_stmt if it's just going to be used here
                ElseStmt::Else(ref eelse) => eelse.accept(self),
                ElseStmt::ElseIf(ref elseif) => elseif.accept(self),
            }
        } else {
            ControlFlowType::None
        }
    }

    fn visit_while_stmt(&self, expr: &WhileStmt) -> Self::Output {
        let eval = ExprEvaluator;

        while match expr.cond.accept(&eval) {
            Value::Number(n) => n > 0.,
            Value::Boolean(b) => b,
            _ => {
                errors::LIST
                    .lock()
                    .unwrap()
                    .push(AstError::BadWhileExpression, None);
                false
            }
        } {
            match expr.then.accept(self) {
                ControlFlowType::Break => break,
                ControlFlowType::Continue => {
                    if let Some(ref stmt) = expr.incr_stmt {
                        stmt.accept(self);
                    }

                    continue;
                }
                ControlFlowType::Return(ret) => {
                    return ControlFlowType::Return(ret);
                }
                _ => {}
            }

            if let Some(ref stmt) = expr.incr_stmt {
                stmt.accept(self);
            }
        }

        ControlFlowType::None
    }

    fn visit_for_stmt(&self, expr: &ForStmt) -> Self::Output {
        if let Some(ref decl) = expr.decl {
            decl.accept(self);
        }

        expr.iwhile.accept(self)
    }

    fn visit_ctrl_stmt(&self, expr: &CtrlStmt) -> Self::Output {
        expr.ctrl.clone()
    }

    fn visit_include_stmt(&self, expr: &IncludeStmt) -> Self::Output{
        let name = &expr.file;

        let inner = match fs::read_to_string(name){
            Ok(var) => var,
            Err(_) => {
                errors::LIST.lock().unwrap().push(AstError::FailedToInclude, None);
                return ControlFlowType::None;
            }
        };

        utils::run_interpreter(inner, name.clone());

        ControlFlowType::None
    }

    fn visit_set_stmt(&self, expr: &SetStmt) -> Self::Output {
        let eval = ExprEvaluator;

        let caller = expr.callee.accept(&eval);

        let insert_into_env = |env: &Mutex<VarMap>| {
            let mut guard = env.lock().unwrap();

            let name = expr.name.token_type.as_identifier().unwrap().clone();

            let rvalue = expr.rvalue.accept(&eval);

            if expr.op.token_type.is_equal() {
                guard.insert(name, rvalue);
                return;
            }

            //is of type
            let lvalue = match guard.get(&name).cloned() {
                Some(val) => val,
                None => Value::Null,
            };

            let result = match expr.op.token_type {
                TokenType::PlusEqual => eval.add(lvalue, rvalue),
                TokenType::MinusEqual => eval.subtract(lvalue, rvalue),
                TokenType::StarEqual => eval.multiply(lvalue, rvalue),
                TokenType::SlashEqual => eval.divide(lvalue, rvalue),
                TokenType::Percentage => eval.modulus(lvalue, rvalue),
                _ => unreachable!(),
            };

            guard.insert(name, result);
        };

        match caller {
            Value::__StructEnv(ref inst) => {
                insert_into_env(inst);
            }
            Value::StructInst(ref inst) => {
                insert_into_env(&inst.env);
            } //we don't allow static set stmts
            _ => {
                errors::LIST
                    .lock()
                    .unwrap()
                    .push(AstError::BadSetExpression, Some(expr.op.clone()));
            }
        }

        ControlFlowType::None
    }

    fn visit_array_set_stmt(&self, expr: &ArraySetStmt) -> Self::Output {
        let eval = ExprEvaluator;

        let caller = expr.callee.accept(&eval);

        let argument = expr.argument.accept(&eval);

        match caller {
            Value::Array(mut arr) => {
                match arr.set_with_value(&argument, expr.rvalue.accept(&eval)) {
                    None => {
                        errors::LIST
                            .lock()
                            .unwrap()
                            .push(AstError::InvalidArrayAccess, None);
                    }
                    Some(_) => {}
                }
            }
            _ => {
                errors::LIST
                    .lock()
                    .unwrap()
                    .push(AstError::BadArraySetExpression, Some(expr.op.clone()));
            }
        }

        ControlFlowType::None
    }
}

pub struct ExprEvaluator;

impl ExprEvaluator {
    fn push_to_arr(&self, l: &Mutex<Vec<Value>>, r: Value) -> Vec<Value> {
        let left = l.lock().unwrap();
        let mut out = (*left).clone();

        match r {
            Value::Boolean(b) => out.push(Value::Boolean(b)),
            Value::Number(n) => out.push(Value::Number(n)),
            Value::Char(c) => out.push(Value::Char(c)),
            Value::Null => out.push(Value::Null),
            _ => unreachable!(),
        }

        out
    }

    fn add(&self, left: Value, right: Value) -> Value {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
            (Value::Array(l), Value::Array(r)) => {
                let left = l.lock().unwrap();
                let right = r.lock().unwrap();

                [(*left).clone(), (*right).clone()].concat().into()
            }
            (Value::Array(l), r) => self.push_to_arr(&l, r).into(),
            (Value::StructInst(l), r) => {
                let func = match l.get_with_this_as_func("_add_") {
                    Some(v) => v,
                    None => {
                        errors::LIST
                            .lock()
                            .unwrap()
                            .push(AstError::InvalidUseOfAddOperator, None);
                        return Value::Null;
                    }
                };

                func.call(&StmtEvaluator, self, vec![r])
            }
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::InvalidUseOfAddOperator, None),
        }
    }

    fn subtract(&self, left: Value, right: Value) -> Value {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l - r),
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::InvalidUseOfSubtractOperator, None),
        }
    }

    fn multiply(&self, left: Value, right: Value) -> Value {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l * r),
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::InvalidUseOfMulOperator, None),
        }
    }

    fn modulus(&self, left: Value, right: Value) -> Value {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l % r),
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::InvalidUseOfModOperator, None),
        }
    }



    fn divide(&self, left: Value, right: Value) -> Value {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => {
                if r == 0.0 {
                    return errors::LIST
                        .lock()
                        .unwrap()
                        .push(AstError::DivisionByZero, None);
                }

                Value::Number(l / r)
            }
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::InvalidUseOfDivOperator, None),
        }
    }

    fn greater(&self, left: Value, right: Value, equal: bool) -> Value {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => {
                Value::Boolean(if equal { l >= r } else { l > r })
            }
            (Value::Boolean(l), Value::Boolean(r)) => {
                Value::Boolean(if equal { l >= r } else { l & !r })
            }
            (Value::Char(l), Value::Char(r)) => Value::Boolean(if equal { l >= r } else { l > r }),
            (Value::Null, Value::Null) => Value::Boolean(equal),
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::InvalidUseOfGreaterOperator, None),
        }
    }

    fn less(&self, left: Value, right: Value, equal: bool) -> Value {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => {
                Value::Boolean(if equal { l <= r } else { l < r })
            }
            (Value::Boolean(l), Value::Boolean(r)) => {
                Value::Boolean(if equal { l <= r } else { !l & r })
            }
            (Value::Char(l), Value::Char(r)) => Value::Boolean(if equal { l <= r } else { l < r }),
            (Value::Null, Value::Null) => Value::Boolean(equal),
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::InvalidUseOfLessOperator, None),
        }
    }

    fn equal(&self, left: Value, right: Value, not: bool) -> Value {
        match (left, right) {
            (Value::StructInst(l), r) => {
                let func = match l.get_with_this_as_func("_eq_") {
                    Some(v) => v,
                    None => {
                        errors::LIST
                            .lock()
                            .unwrap()
                            .push(AstError::InvalidUseOfEqualOperator, None);
                        return Value::Null;
                    }
                };

                func.call(&StmtEvaluator, self, vec![r])
            },
            (Value::Array(l), Value::Array(r)) => {
                let left = l.lock().unwrap();
                let right = r.lock().unwrap();

                if left.len() != right.len(){
                    return Value::Boolean(not);
                }

                let result = left.iter().zip(right.iter()).filter(|&(a,b)| {
                    match self.equal(a.clone(), b.clone(), false){
                        Value::Boolean(b) => b,
                        _ => false
                    }
                }).count() == left.len();

                Value::Boolean(if not{ !result } else { result })
            },
            (Value::Boolean(l), Value::Boolean(r)) => {
                Value::Boolean(if not { l != r } else { l == r })
            }
            (Value::Char(l), Value::Char(r)) => Value::Boolean(if not { l != r } else { l == r }),
            (Value::Number(l), Value::Number(r)) => {
                Value::Boolean(if not { l != r } else { l == r })
            }
            (Value::Boolean(_), Value::Null) | (Value::Null, Value::Boolean(_)) | (Value::Char(_), Value::Null) | (Value::Null, Value::Char(_)) => {
                Value::Boolean(not)
            }
            (Value::Number(n), Value::Null) | (Value::Null, Value::Number(n)) => {
                Value::Boolean(if not { n != 0.0 } else { n == 0.0 })
            }
            (Value::Null, Value::Null) => Value::Boolean(!not),
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::InvalidUseOfEqualOperator, None),
        }
    }

    fn and(&self, left: Value, right: Value) -> Value {
        match (left, right) {
            (Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(l && r),
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::InvalidUseOfAndOperator, None),
        }
    }

    fn or(&self, left: Value, right: Value) -> Value {
        match (left, right) {
            (Value::Boolean(l), Value::Boolean(r)) => Value::Boolean(l || r),
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::InvalidUseOfOrOperator, None),
        }
    }
}

impl ExprVisitor for ExprEvaluator {
    type Output = Value;

    //this could be part of unary, decided not to as it would clutter the grammar
    fn visit_cast_expr(&self, expr: &CastExpr) -> Self::Output {
        let right = expr.argument.accept(self);

        match expr.op.token_type {
            TokenType::BoolCast => right.cast_to_bool(),
            TokenType::NumberCast => right.cast_to_number(),
            TokenType::CharCast => right.cast_to_char(),
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::BadCastingType, Some(expr.op.clone())),
        }
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Self::Output {
        let op = expr.op.clone();
        let right = expr.right.accept(self);

        match op.token_type {
            TokenType::Minus => match right {
                Value::Number(n) => Value::Number(-n),
                _ => errors::LIST
                    .lock()
                    .unwrap()
                    .push(AstError::NegationError, Some(op)),
            },
            TokenType::Not => match right {
                Value::Boolean(b) => Value::Boolean(!b),
                _ => errors::LIST
                    .lock()
                    .unwrap()
                    .push(AstError::NotError, Some(op)),
            },
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::UnknownUnaryOperator, Some(op)),
        }
    }

    fn visit_binary_expr(&self, expr: &BinaryExpr) -> Self::Output {
        let left = expr.left.accept(self);
        let right = expr.right.accept(self);

        match expr.op.token_type {
            TokenType::Plus | TokenType::PlusEqual => self.add(left, right),
            TokenType::Minus | TokenType::MinusEqual => self.subtract(left, right),
            TokenType::Star => self.multiply(left, right),
            TokenType::Percentage => self.modulus(left, right),
            TokenType::Slash => self.divide(left, right),
            TokenType::Greater => self.greater(left, right, false),
            TokenType::GreaterEqual => self.greater(left, right, true),
            TokenType::Less => self.less(left, right, false),
            TokenType::LessEqual => self.less(left, right, true),
            TokenType::EqualEqual => self.equal(left, right, false),
            TokenType::NotEqual => self.equal(left, right, true),
            TokenType::And => self.and(left, right),
            TokenType::Or => self.or(left, right),
            _ => errors::LIST
                .lock()
                .unwrap()
                .push(AstError::UnknownBinaryOperator, None),
        }
    }

    fn visit_array_expr(&self, expr: &ArrayExpr) -> Self::Output {
        let mut out_vec = Vec::<Value>::new();

        let exprs = &expr.exprs;

        for expr in exprs {
            out_vec.push(expr.accept(self));
        }

        out_vec.into()
    }

    fn visit_literal_expr(&self, expr: &LiteralExpr) -> Self::Output {
        expr.value.clone()
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Self::Output {
        expr.expr.accept(self)
    }

    fn visit_variable_expr(&self, expr: &VarExpr) -> Self::Output {
        let tok = &expr.name;

        let name = tok
            .token_type
            .as_identifier()
            .expect("Failure converting to identifier at the parser");

        VARMAP
            .lock()
            .unwrap()
            .get(name)
            .cloned()
            .unwrap_or(Value::Null)
    }

    fn visit_array_get_expr(&self, expr: &ArrayGetExpr) -> Self::Output {
        let eval = ExprEvaluator;

        let callee = expr.callee.accept(&eval);

        let argument = expr.argument.accept(&eval);

        if !callee.is_array() {
            errors::LIST
                .lock()
                .unwrap()
                .push(AstError::InvalidArrayAccess, None);

            return Value::Null;
        }

        let callee = callee.as_array().unwrap();

        match callee.get_with_value(&argument) {
            None => {
                errors::LIST
                    .lock()
                    .unwrap()
                    .push(AstError::InvalidArrayAccess, None);

                Value::Null
            }
            Some(val) => val,
        }
    }

    fn visit_call_expr(&self, expr: &CallExpr) -> Self::Output {
        let eval = StmtEvaluator;

        let caller = expr.callee.accept(self);

        if !caller.is_function() && !caller.is_struct_def() {
            errors::LIST
                .lock()
                .unwrap()
                .push(AstError::BadFunctionCall, expr.paren_tok.clone());

            return Value::Null;
        }

        let args = expr
            .arguments
            .iter()
            .map(|each| each.accept(self))
            .collect();

        match caller {
            Value::Function(ref func) => func.call(&eval, self, args),
            Value::StructDef(ref def) => def.call(&eval, self, args),
            _ => unreachable!(),
        }
    }

    fn visit_get_expr(&self, expr: &GetExpr) -> Self::Output {
        let caller = expr.callee.accept(self);

        let argument = &expr
            .argument
            .token_type
            .as_identifier()
            .unwrap()
            .to_string();

        match caller {
            Value::StructInst(ref inst) => {
                if expr.is_static {
                    errors::LIST
                        .lock()
                        .unwrap()
                        .push(AstError::BadGetExpression, None);
                    return Value::Null;
                }

                inst.get_with_this(argument).unwrap_or(Value::Null)
            }
            Value::__StructEnv(ref env) => {
                //a 'this' expression
                if expr.is_static {
                    errors::LIST
                        .lock()
                        .unwrap()
                        .push(AstError::BadGetExpression, None);
                    return Value::Null;
                }

                env.lock()
                    .unwrap()
                    .get(argument)
                    .unwrap_or(&Value::Null)
                    .clone()
            }
            Value::StructDef(ref def) => {
                if !expr.is_static {
                    errors::LIST
                        .lock()
                        .unwrap()
                        .push(AstError::BadStaticGetExpression, None);
                    return Value::Null;
                }

                def.env
                    .lock()
                    .unwrap()
                    .get(argument)
                    .unwrap_or(&Value::Null)
                    .clone()
            }
            Value::Array(ref arr) => arr.get_internal_var(argument),
            _ => {
                errors::LIST
                    .lock()
                    .unwrap()
                    .push(AstError::BadGetExpression, None);

                Value::Null
            }
        }
    }

    fn visit_this_expr(&self, expr: &ThisExpr) -> Self::Output {
        let guard = VARMAP.lock().unwrap();

        match guard.get("this") {
            Some(val) => val.clone(),
            None => {
                errors::LIST
                    .lock()
                    .unwrap()
                    .push(AstError::BadThisExpression, Some(expr.token.clone()));
                Value::Null
            }
        }
    }
}

impl ExprEvaluator {
    pub fn work(&self, expr: Expression) -> Value {
        expr.accept(self)
    }
}

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        let funcs: Vec<Box<dyn Callable>> =
            vec![Box::new(Clock), Box::new(Print), Box::new(PrintLn), Box::new(TypeOf), Box::new(_TypeOf)];

        let mut var_map = VARMAP.lock().unwrap();

        for func in funcs{
            var_map.insert(func.name(), Value::Function(func)); // map the global functions
        }

        Interpreter
    }

    pub fn work(&self, statements: Vec<Statement>) {
        let worker = StmtEvaluator;

        for stmt in statements {
            stmt.accept(&worker);

            errors::LIST.lock().unwrap().report();
        }
    }
}
