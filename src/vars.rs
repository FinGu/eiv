use std::{
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

use crate::ast::structs::Value;

type MapType = HashMap<String, Value>;

#[derive(Debug, Clone)]
pub struct VarMap {
    pub inner: Option<Box<VarMap>>,
    pub list: MapType,
    pub is_func: bool,
}

impl VarMap {
    pub fn new(is_func: bool, list: MapType, inner: Option<Box<VarMap>>) -> Self {
        Self {
            list,
            inner,
            is_func,
        }
    }

    pub fn get_last_inner(&mut self) -> Option<&mut Box<VarMap>> {
        let mut cur = &mut self.inner;

        if cur.is_none() {
            return None;
        }

        while cur.as_ref().unwrap().inner.is_some() {
            cur = &mut cur.as_mut().unwrap().inner;
        }

        match cur {
            Some(val) => Some(val),
            None => None,
        }
    }

    pub fn set_inner(&mut self, inner: Option<Box<VarMap>>) {
        let mut cur = &mut self.inner;

        while let Some(inn) = cur {
            cur = &mut inn.inner;
        }

        *cur = inner;
    }

    // https://users.rust-lang.org/t/how-do-you-remove-the-last-node-from-a-singly-linked-list/31805/7
    pub fn del_inner(&mut self) {
        let mut cur = &mut self.inner;

        while cur.as_ref().unwrap().inner.is_some() {
            cur = &mut cur.as_mut().unwrap().inner;
        }

        *cur = None;
    }

    pub fn get(&self, k: &str) -> Option<&Value> {
        /*if let Some(first) = self.list.get(k){
            return Some(first);
        }*/

        self.inner
            .as_ref()
            .and_then(|hier| hier.get(k))
            .or_else(|| self.list.get(k))
    }

    pub fn insert(&mut self, k: String, v: Value) -> Option<Value> {
        if let Some(last) = self.get_last_inner() {
            if last.is_func {
                return last.list.insert(k, v);
            }
        } 
        // don't mess with scopes outside of the function

        if self.list.contains_key(&k) {
            return self.insert_to_cur(k, v);
        }
        
        if let Some(ref mut hier) = self.inner {
            return hier.insert(k, v);
        }

        self.insert_to_cur(k, v)
    }

    pub fn insert_to_cur(&mut self, k: String, v: Value) -> Option<Value> {
        self.list.insert(k, v)
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

pub static VARMAP: LazyLock<Mutex<VarMap>> =
    LazyLock::new(|| Mutex::new(VarMap::new(false, HashMap::new(), None)));

pub fn create_inner(is_func: bool) {
    let new_map = VarMap::new(is_func, HashMap::new(), None);

    set_inner(Box::new(new_map));
}

pub fn delete_inner() {
    VARMAP.lock().unwrap().del_inner();
}

pub fn set_inner(map: Box<VarMap>) {
    VARMAP.lock().unwrap().set_inner(Some(map));
}

pub fn clone_inner() -> Option<Box<VarMap>> {
    VARMAP.lock().unwrap().get_last_inner().map(|inn| inn.clone())
}
