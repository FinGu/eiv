use std::{
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

use crate::ast::structs::Value;

pub struct Environment(pub VarMap);

type MapType = HashMap<String, Value>;

#[derive(Debug, Clone)]
pub struct VarMap {
    pub inner: Option<Box<VarMap>>,
    pub list: MapType,
}

impl VarMap {
    pub fn new(inner: Option<Box<VarMap>>) -> Self {
        Self {
            list: MapType::new(),
            inner,
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        match self.list.get(name){
            Some(value) => Some(value.clone()),
            None => match &self.inner{
                Some(inner) => inner.get(name),
                None => None,
            }
        }
    }

    pub fn insert(&mut self, k: String, v: Value) -> Option<Value> {
        self.list.insert(k, v)
    }

    pub fn remove(&mut self, k: &str) {
        self.list.remove(k);
    }

    pub fn print_hierarchy(&self) {
        println!("{:?}", self.list);

        let mut cur = &self.inner;

        while let Some(inn) = cur {
            println!("{:?}", inn.list);

            cur = &inn.inner;
        }
    }
}

pub fn set_environment(new_env: VarMap){
    VARMAP.lock().unwrap().0 = new_env;
}

pub fn clone_environment() -> VarMap{
    VARMAP.lock().unwrap().0.clone()
}

pub fn insert(name: String, val: Value){
    VARMAP.lock().unwrap().0.insert(name, val);
}

pub fn get(name: &str) -> Option<Value>{
    VARMAP.lock().unwrap().0.get(name)
}

pub static VARMAP: LazyLock<Mutex<Environment>> =
    LazyLock::new(|| Mutex::new(Environment(VarMap::new(None))));

