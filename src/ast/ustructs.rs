use std::{
    fmt::Display, ops::Deref, sync::{Arc, Mutex}
};

use super::{
    funcs::UserFunction, structs::{AstError, Callable, Statement, Value}, StmtEvaluator, __execute_block 
};

use crate::{
    errors,
    vars::{self, VarMap},
};

#[derive(Clone, Debug)]
pub struct UserStructDef {
    pub inst_methods_and_fields: Vec<Statement>, //Statement and if it is static
    pub env: VarMap,
}

#[derive(Clone, Debug)]
pub struct UserStructInst(pub Arc<Mutex<VarMap>>); 

impl UserStructDef {
    fn accept_static(stmt_eval: &StmtEvaluator, map: &Vec<Statement>) -> VarMap {
        let mut env = vars::clone_environment();

        __execute_block(stmt_eval, map, &mut env);

        env 
    }

    pub fn new(stmt_eval: &StmtEvaluator, methods: &Vec<Statement>) -> Self {
        let mut out_map = Vec::<Statement>::new();

        let mut inst_out_map = Vec::<Statement>::new();

        for stmt in methods {
            //extract all functions and handle differently if are static
            let result = match stmt {
                Statement::Variable(_) | Statement::Function(_) | Statement::Struct(_) => {
                    Some(stmt)
                }
                Statement::Static(ref inn) => {
                    out_map.push(inn.inner.clone());
                    None
                }
                _ => {
                    errors::LIST
                        .lock()
                        .unwrap()
                        .push(AstError::BadStmtInsideStruct, None);
                    None
                }
            };

            if let Some(res) = result {
                inst_out_map.push(res.clone());
            }
        }

        let env = UserStructDef::accept_static(stmt_eval, &out_map);

        Self {
            inst_methods_and_fields: inst_out_map,
            env,
        }
    }
}

impl Callable for UserStructDef {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        stmt_eval: &super::StmtEvaluator,
        expr_eval: &super::ExprEvaluator,
        args: Vec<Value>,
    ) -> Value {
        let mut new_map = VarMap::new(Some(Box::new(self.env.clone())));

        __execute_block(stmt_eval, &self.inst_methods_and_fields, &mut new_map);

        let inst = UserStructInst::new(new_map);

        let cloned_function = inst.get("constructor", true);

        if let Some(cons) = cloned_function {
            match cons {
                Value::Function(ref fun) => {
                    fun.bind(&inst).call(stmt_eval, expr_eval, args);
                }
                _ => {
                    errors::LIST
                        .lock()                        
                        .unwrap()
                        .push(AstError::ConstructorMustBeFunc, None);
                }
            }
        }

        inst.into()
    }

    fn name(&self) -> String {
        unimplemented!()
    }

    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(self.clone())
    }
}

impl From<UserStructDef> for Value {
    fn from(value: UserStructDef) -> Self {
        Value::StructDef(value)
    }
}

impl From<UserStructInst> for Value {
    fn from(value: UserStructInst) -> Self {
        Value::StructInst(value)
    }
}

impl UserStructInst{
    pub fn new(inst: VarMap) -> Self{
        Self(Arc::new(Mutex::new(inst)))
    }

    pub fn get(&self, argument: &str, with_this: bool) -> Option<Value>{
        self.0.lock().unwrap().get(argument).map(|val| if val.is_function() && with_this{
            val.as_function().unwrap().bind(self).into()
        } else{
            val
        })
    }

    pub fn set(&mut self, argument: String, val: Value){
        self.0.lock().unwrap().insert(argument, val);
    }
    
    pub fn get_with_this(&mut self, argument: &str) -> Option<Value> {
        self.get(argument, true)
    }

    pub fn get_with_this_as_func(&mut self, argument: &str) -> Option<UserFunction> {
        let val = self.get(argument, true)?;

        if !val.is_function() {
            return None;
        }

        Some(val.as_function().unwrap().clone())
    }

}

#[derive(Clone, Debug)]
pub struct UserArray {
    arr: Arc<Mutex<Vec<Value>>>,
}

impl UserArray {
    pub fn new(vec: Vec<Value>) -> Self {
        Self {
            arr: Arc::new(Mutex::new(vec)),
        }
    }

    pub fn get_internal_var(&self, name: &str) -> Value {
        let arr = self.arr.lock().unwrap();

        match name {
            "length" => Value::Number(arr.len() as f64),
            _ => Value::Null,
        }
    }

    pub fn get(&self, index: usize) -> Value {
        self.arr.lock().unwrap()[index].clone()
    }

    pub fn get_with_value(&self, value: &Value) -> Option<Value> {
        let len = self.arr.lock().unwrap().len();

        let index = value.as_index(len as i32);

        index.map(|i| self.get(i))
    }

    pub fn set(&mut self, index: usize, value: Value) {
        self.arr.lock().unwrap()[index] = value;
    }

    pub fn set_with_value(&mut self, argument: &Value, value: Value) -> Option<()> {
        let len = self.arr.lock().unwrap().len();

        let index = argument.as_index(len as i32);

        index.map(|i| self.set(i, value))
    }
}

impl Display for UserArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let larr = self.arr.lock().unwrap();

        write!(f, "[")?;

        for (i, value) in larr.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }

            match value {
                Value::Char(_) => write!(f, "'{}'", value)?,
                _ => write!(f, "{}", value)?,
            }
        }

        write!(f, "]")
    }
}

impl Deref for UserArray {
    //bad practice
    type Target = Arc<Mutex<Vec<Value>>>;

    fn deref(&self) -> &Self::Target {
        &self.arr
    }
}
