use std::{
    collections::HashMap,
    sync::{Arc, LazyLock, Mutex},
};

use crate::ast::structs::Value;

pub type SharedMap = Arc<Mutex<VarMap>>;

pub struct Environment(pub SharedMap);

impl Environment{
    pub fn new(env: VarMap) -> Self{
        Self(env.into())
    }
}

type MapType = HashMap<String, Value>;

#[derive(Debug, Clone)]
pub struct VarMap {
    pub inner: Option<SharedMap>,
    pub list: MapType,
}

impl VarMap {
    pub fn new(inner: Option<SharedMap>) -> Self {
        Self {
            list: MapType::new(),
            inner,
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        match self.list.get(name){
            Some(value) => Some(value.clone()),
            None => match &self.inner{
                Some(inner) => inner.lock().unwrap().get(name),
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

    /*pub fn print_hierarchy(&self) {
        println!("{:?}", self.list);

        let mut cur = &self.inner;

        while let Some(inn) = cur {
            println!("{:?}", inn.list);

            cur = &inn.inner;
        }
    }*/
}

impl From<VarMap> for SharedMap{
    fn from(value: VarMap) -> Self {
        Arc::new(Mutex::new(value))
    }
}

pub fn set_environment(new_env: SharedMap){
    VARMAP.lock().unwrap().0 = new_env;
}

pub fn clone_environment() -> SharedMap{
    VARMAP.lock().unwrap().0.clone()
}

pub fn insert(name: String, val: Value){
    VARMAP.lock().unwrap().0.lock().unwrap().insert(name, val);
}

pub fn get(name: &str) -> Option<Value>{
    VARMAP.lock().unwrap().0.lock().unwrap().get(name)
}

pub static VARMAP: LazyLock<Mutex<Environment>> =
    LazyLock::new(|| Mutex::new(Environment::new(VarMap::new(None))));

