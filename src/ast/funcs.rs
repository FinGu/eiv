use std::{
    collections::HashMap,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    ast::{__execute_block, structs::AstError},
    errors,
    vars::{self, VarMap},
};

use super::{
    structs::{Accept, Callable, ControlFlowType, Statement, Value}, ustructs::UserArray, ExprEvaluator, StmtEvaluator
};

#[derive(Copy, Clone)]
pub struct Clock;

impl Callable for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: &StmtEvaluator, _: &ExprEvaluator, _: Vec<Value>) -> Value {
        Value::Number(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        )
    }

    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(*self)
    }

    fn name(&self) -> String {
        String::from("clock")
    }
}

#[derive(Copy, Clone)]
pub struct Print;

impl Callable for Print {
    fn arity(&self) -> usize {
        255
    }

    fn call(
        &self,
        stmt_eval: &StmtEvaluator,
        expr_eval: &ExprEvaluator,
        args: Vec<Value>,
    ) -> Value {
        for arg in &args {
            if let Value::StructInst(inst) = arg {
                if let Some(fun) = inst.get_with_this_as_func("_display_"){
                    fun.call(stmt_eval, expr_eval, Vec::new());
                    continue;
                }
            }
            print!("{}", arg);
        }

        Value::Null
    }

    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(*self)
    }

    fn name(&self) -> String {
        String::from("print")
    }
}

#[derive(Copy, Clone)]
pub struct PrintLn;

impl Callable for PrintLn {
    fn arity(&self) -> usize {
        255
    }

    fn call(
        &self,
        stmt_eval: &StmtEvaluator,
        expr_eval: &ExprEvaluator,
        args: Vec<Value>,
    ) -> Value {
        let prnt = Print;

        prnt.call(stmt_eval, expr_eval, args);

        println!();

        Value::Null
    }

    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(*self)
    }

    fn name(&self) -> String {
        String::from("println")
    }
}

#[derive(Copy, Clone)]
pub struct _TypeOf;

impl Callable for _TypeOf {
    fn arity(&self) -> usize {
        1 
    }

    fn call(
        &self,
        _: &StmtEvaluator,
        _: &ExprEvaluator,
        args: Vec<Value>,
    ) -> Value {
        if args.len() != self.arity() {
            errors::LIST
                .lock()
                .unwrap()
                .push(AstError::WrongNumArgsFun, None);
            return Value::Null;
        }

        let text_result = match &args[0]{
            Value::Char(_) => "char",
            Value::Array(_) => "array",
            Value::Number(_) => "number",
            Value::Boolean(_) => "bool",
            Value::Function(_) => "function",
            Value::StructDef(_) => "struct_definition",
            Value::StructInst(_) | Value::__StructEnv(_) => "struct_instance",
            Value::Null => "null"
        };

        let as_value_vec = text_result.
            as_bytes()
            .iter()
            .map(|e| Value::Char(*e))
            .collect::<Vec<Value>>();

        Value::Array(UserArray::new(as_value_vec))
    }

    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(*self)
    }

    fn name(&self) -> String {
        String::from("_typeof")
    }
}


#[derive(Copy, Clone)]
pub struct TypeOf;

impl Callable for TypeOf {
    fn arity(&self) -> usize {
        1 
    }

    fn call(
        &self,
        stmt_eval: &StmtEvaluator,
        expr_eval: &ExprEvaluator,
        args: Vec<Value>,
    ) -> Value {
        if args.len() != self.arity() {
            errors::LIST
                .lock()
                .unwrap()
                .push(AstError::WrongNumArgsFun, None);
            return Value::Null;
        }

        match args[0]{
            Value::__StructEnv(ref inst) => {
                let linst = inst.lock().unwrap();

                if let Some(fun) = linst.get("_type_"){
                    if !fun.is_array() {
                        errors::LIST
                            .lock()
                            .unwrap()
                            .push(AstError::OverloadNotOfRightType, None);
                        return Value::Null;
                    }

                    return fun
                        .as_array().unwrap()
                        .lock().unwrap()
                        .clone().into();
                }
            }
            Value::StructInst(ref inst) => {
                if let Some(fun) = inst.get("_type_", false){
                    if !fun.is_array() {
                        errors::LIST
                            .lock()
                            .unwrap()
                            .push(AstError::OverloadNotOfRightType, None);
                        return Value::Null;
                    }

                    let as_arr = fun.as_array().unwrap().lock().unwrap();

                    return as_arr.clone().into();
                }
            },
            _ => {}
        }

        let _typeof = _TypeOf;

        _typeof.call(stmt_eval, expr_eval, args)
    }

    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(*self)
    }

    fn name(&self) -> String {
        String::from("typeof")
    }
}



#[derive(Clone)]
pub struct UserFunction {
    pub params: Vec<String>,
    pub body: Vec<Statement>,
}

impl UserFunction {
    pub fn new(params: Vec<String>, body: Vec<Statement>) -> Self {
        Self { params, body }
    }
}

impl Callable for UserFunction {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(
        &self,
        stmt_eval: &StmtEvaluator,
        expr_eval: &ExprEvaluator,
        args: Vec<Value>,
    ) -> Value {
        if args.len() != self.params.len() {
            errors::LIST
                .lock()
                .unwrap()
                .push(AstError::WrongNumArgsFun, None);
            return Value::Null;
        }

        let mut env = Box::new(VarMap::new(true, HashMap::new(), None));

        for (param, value) in self.params.iter().zip(args.into_iter()) {
            env.insert_to_cur(param.clone(), value);
        }

        vars::set_inner(env);

        let cflow = __execute_block(stmt_eval, &self.body);

        match cflow {
            ControlFlowType::Return(val) => {
                let result = val.accept(expr_eval);

                vars::delete_inner(); //we delete after evaluating the return

                result
            }
            ControlFlowType::None => {
                vars::delete_inner();

                Value::Null
            }
            _ => {
                errors::LIST
                    .lock()
                    .unwrap()
                    .push(AstError::InvalidControlFlowStmt, None);

                vars::delete_inner();

                Value::Null
            }
        }
    }

    fn name(&self) -> String {
        unimplemented!()
    }

    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(self.clone())
    }
}
