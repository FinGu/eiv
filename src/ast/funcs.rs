use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    ast::structs::AstError,
    errors,
    vars::{self, VarMap},
};

use super::{
    structs::{Accept, Callable, ControlFlowType, FnStmt, Value}, ustructs::{UserArray, UserStructInst}, ExprEvaluator, StmtEvaluator, __execute_block_without_cleanup
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
        mut args: Vec<Value>,
    ) -> Value {
        for arg in &mut args {
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
            Value::GlobalFunction(_) => "global_function",
            Value::StructDef(_) => "struct_definition",
            Value::StructInst(_) => "struct_instance",
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

        let mut args = args;

        match args[0]{
            Value::StructInst(ref mut inst) => {
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

#[derive(Clone, Debug)]
pub struct UserFunction {
    pub decl: FnStmt,
    pub env: VarMap,
}

impl UserFunction {
    pub fn new(decl: FnStmt, env: VarMap) -> Self {
        Self { decl, env }
    }

    pub fn bind(&self, inst: &UserStructInst) -> Self{
        let in_use_env = vars::clone_environment();
    
        let mut new_env = VarMap::new(Some(Box::new(in_use_env)));
    
        new_env.insert(String::from("this"), Value::StructInst(inst.clone()));
        //method env -> instance env -> global env 
    
        UserFunction::new(self.decl.clone(), new_env)
    }
}

impl Callable for UserFunction {
    fn arity(&self) -> usize {
        self.decl.params.len()
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

        let mut env = VarMap::new(Some(Box::new(self.env.clone())));

        for (param, value) in self.decl.params.iter().zip(args.into_iter()) {
            env.insert(param.clone(), value);
        }

        let old_map = vars::clone_environment();

        let cflow = __execute_block_without_cleanup(stmt_eval, &self.decl.body, &mut env);

        let result = match cflow{
            ControlFlowType::Return(val) => val.accept(expr_eval),
            ControlFlowType::None => Value::Null,
            _ => {
                errors::LIST
                    .lock()
                    .unwrap()
                    .push(AstError::InvalidControlFlowStmt, None);
                
                Value::Null
            }
        };

        vars::set_environment(old_map);

        result
    }

    fn name(&self) -> String {
        unimplemented!()
    }

    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(self.clone())
    }
}
