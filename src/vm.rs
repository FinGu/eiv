use std::{cmp::Ordering, collections::HashMap, ops::{Add, Div, Mul, Sub}};

use enum_as_inner::EnumAsInner;

#[derive(Clone, Debug, EnumAsInner)]
pub enum Immediate{
    Number(f64),
    Char(u8),
    Boolean(bool),
    Array(Vec<Immediate>),
    Null,
}

impl From<f64> for Immediate{
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<u8> for Immediate{
    fn from(value: u8) -> Self {
        Self::Char(value)
    }
}

impl From<bool> for Immediate{
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<Vec<Immediate>> for Immediate{
    fn from(value: Vec<Immediate>) -> Self {
        Self::Array(value)
    }
}

impl PartialOrd for Immediate {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Null, Self::Null) => Some(std::cmp::Ordering::Equal),
            (Self::Number(n1), Self::Number(n2)) => n1.partial_cmp(n2),
            (Self::Char(c1), Self::Char(c2)) => c1.partial_cmp(c2),
            (Self::Boolean(b1), Self::Boolean(b2)) => b1.partial_cmp(b2),
            (Self::Array(arr1), Self::Array(arr2)) => {
                if arr1.len() != arr2.len() {
                    return arr1.len().partial_cmp(&arr2.len());
                }
                
                for (a, b) in arr1.iter().zip(arr2.iter()) {
                    match a.partial_cmp(b) {
                        Some(std::cmp::Ordering::Equal) => continue,
                        other => return other,
                    }
                }
                Some(std::cmp::Ordering::Equal)
            },
            (Self::Null, _) => Some(std::cmp::Ordering::Less),
            (_, Self::Null) => Some(std::cmp::Ordering::Greater),
            (Self::Number(_), _) => Some(std::cmp::Ordering::Less),
            (_, Self::Number(_)) => Some(std::cmp::Ordering::Greater),
            (Self::Char(_), _) => Some(std::cmp::Ordering::Less),
            (_, Self::Char(_)) => Some(std::cmp::Ordering::Greater),
            (Self::Boolean(_), _) => Some(std::cmp::Ordering::Less),
            (_, Self::Boolean(_)) => Some(std::cmp::Ordering::Greater),
        }
    }
}

impl PartialEq for Immediate{
    fn eq(&self, other: &Self) -> bool {
        match (self, other){
            (Self::Null, Self::Null) => true,
            (Self::Number(n1), Self::Number(n2)) => n1 == n2,
            (Self::Char(c1), Self::Char(c2)) => c1 == c2,
            (Self::Array(arr1), Self::Array(arr2)) => {
                arr1.iter().zip(arr2.iter()).filter(|&(a,b)| a == b).count() == arr1.len()
            }
            _ => false 
        }
    }
}

impl Add for Immediate{
    type Output = Immediate;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs){
            (Self::Number(l), Self::Number(r)) => Self::Number(l + r),
            (Self::Array(mut arr), r) => Self::Array({
                arr.push(r);
                arr
            }),
            _ => Self::Number(f64::NAN) 
        }
    }
}

impl Sub for Immediate{
    type Output = Immediate;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs){
            (Self::Number(l), Self::Number(r)) => Self::Number(l - r),
            _ => Self::Number(f64::NAN)
        }
    }
}

impl Div for Immediate{
    type Output = Immediate;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs){
            (Self::Number(l), Self::Number(r)) => Self::Number(l / r),
            _ => Self::Number(f64::NAN)
        }    
    }
}

impl Mul for Immediate{
    type Output = Immediate;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs){
            (Self::Number(l), Self::Number(r)) => Self::Number(l * r),
            (Self::Char(l), Self::Number(r)) => Self::Array({
                (0..r as usize).map(|_| l.into()).collect()
            }),
            (Self::Array(_l), Self::Number(_r)) => Self::Array({
                //maybe later
                unimplemented!()
            }),
            _ => Self::Number(f64::NAN)

        }
    }
}

impl From<Vec<u8>> for Immediate{
    fn from(value: Vec<u8>) -> Self {
        let new_vec = value.iter().map(|el| Self::Char(*el)).collect();
        
        Self::Array(new_vec)
    }
}

impl Immediate{
    pub fn as_index(&self, bounds: i32) -> Option<usize> {
        if !self.is_number() {
            return None;
        }

        let argument = *self.as_number().unwrap() as i32;

        let index = if argument < 0 { // allow for negative indexing
            bounds + argument
        } else {
            argument
        };

        if index < 0 || index >= bounds {
            return None;
        }

        Some(index as usize)
    }

    pub fn cast_to_bool(&self) -> Self{
        match *self {
            Self::Boolean(b) => Self::Boolean(b),
            Self::Number(n) => Self::Boolean(n != 0.0),
            Self::Char(c) => Self::Boolean(c != 0),
            Self::Null => Self::Boolean(false),
            _ => Self::Boolean(true),
        }
    }

    pub fn cast_to_number(&self) -> Self {
        match *self {
            Self::Number(n) => Self::Number(n),
            Self::Boolean(b) => Self::Number(if b { 1.0 } else { 0.0 }),
            Self::Char(c) => Self::Number(c as f64),
            _ => self.cast_to_bool().cast_to_number(),
        }
    }

    pub fn cast_to_char(&self) -> Self {
        match *self {
            Self::Char(c) => Self::Char(c),
            _ => Self::Null,
        }
    }
}

#[derive(Debug, Clone)]
pub enum OpCode{
    Pop,
    Return,

    Not,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,

    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    BoolCast,
    NumberCast,
    CharCast,

    JumpIfFalse(i32),
    Jump(i32),

    SetLocal(String, usize),
    GetLocal(String, usize),

    TmpPrint,
    Constant(Immediate)
}

#[derive(Debug)]
pub struct Scope{
    data: HashMap<String, Immediate>    
}

impl Scope{
    pub fn new() -> Self {
        Scope {
            data: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: String, value: Immediate) {
        self.data.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&Immediate> {
        self.data.get(key)
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.data.contains_key(key)
    }

    pub fn remove(&mut self, key: &str) -> Option<Immediate> {
        self.data.remove(key)
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }
}

//scopes[0] = globals
pub struct VirtualMachine{
    stack: Vec<Immediate>,
    scopes: Vec<Scope>,
    ip: i32,
}

impl VirtualMachine{
    pub fn new() -> Self{
        Self{
            ip: 0,
            stack: Vec::new(),
            scopes: vec![Scope::new()],
        }
    }

    /*fn get_local(&self, name: &str, scope: usize) -> VMResult<Immediate> {
        self.scopes.get(scope);

        if let Some(map) = self.scopes.get(scope){

        }

        Ok(Immediate::Null)
    }*/

    fn binary_op(&mut self, kind: &OpCode) -> VMResult<Immediate>{ 
        let right = self.stack.pop().unwrap();

        let left = self.stack.pop().unwrap();

        Ok(match kind{
                OpCode::Add => {
                    left + right
                },
                OpCode::Subtract =>{
                    left - right
                },
                OpCode::Multiply =>{
                    left * right
                },
                OpCode::Divide =>{
                    left / right
                },
                OpCode::Equal =>{
                    (left == right).into()
                },
                OpCode::NotEqual => {
                    (left != right).into()
                }
                OpCode::Less => {
                    (left < right).into()
                }
                OpCode::LessEqual =>{
                    (left <= right).into()
                },
                OpCode::Greater => {
                    (left > right).into()
                }
                OpCode::GreaterEqual => {
                    (left >= right).into()
                },
                _ => {
                    return Err(VirtualMachineError::InvalidInpBinaryOp);
                }
            })
    }

    pub fn work(&mut self, instructions: &[OpCode]) -> VMResult<()>{
        self.ip = 0;

        let len = instructions.len() as i32;

        println!("{:?}", instructions);

        while self.ip < len{
            let el = &instructions[self.ip as usize];

            match el{
                OpCode::Pop => {
                    self.stack.pop();
                },
                OpCode::Return =>{
                    self.stack.pop();
                },
                OpCode::Not => {
                    let popped = self.stack.pop().unwrap();

                    self.stack.push(match popped{
                           Immediate::Boolean(b) => Immediate::Boolean(!b),
                           _ => Immediate::Null
                    });
                },
                OpCode::Negate => {
                    let popped = self.stack.pop().unwrap();

                    self.stack.push(match popped{
                        Immediate::Number(n) => Immediate::Number(-n),
                        _ => Immediate::Null
                    })
                },
                OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide | 
                OpCode::Equal | OpCode::NotEqual | OpCode::Less | OpCode::LessEqual | OpCode::Greater | OpCode::GreaterEqual
                    =>{
                    let result = self.binary_op(el);

                    self.stack.push(match result{
                        Ok(ok) => ok,
                        Err(e) => return Err(e)
                    });
                }
                OpCode::NumberCast => {
                    let popped = self.stack.pop().unwrap();

                    self.stack.push(popped.cast_to_number());
                },
                OpCode::BoolCast => {
                    let popped = self.stack.pop().unwrap();

                    self.stack.push(popped.cast_to_bool());
                },
                OpCode::CharCast => {
                    let popped = self.stack.pop().unwrap();

                    self.stack.push(popped.cast_to_char());
                },
                OpCode::Constant(c) =>{
                    self.stack.push(c.clone());
                },
                OpCode::GetLocal(name, scope) => {
                    let scope = *scope;

                    if scope == self.scopes.len(){
                        self.scopes.push(Scope::new());
                    }

                    let data = self.scopes[scope].get(name)
                        .cloned()
                        .unwrap_or(Immediate::Null);

                    self.stack.push(data);
                },
                OpCode::SetLocal(name, scope) =>{
                    let scope = *scope;

                    if scope == self.scopes.len(){
                        self.scopes.push(Scope::new());
                    }

                    self.scopes[scope].insert(name.clone(), self.stack.pop().unwrap());
                },
                OpCode::JumpIfFalse(offset) => {
                    if let Some(Immediate::Boolean(cond)) = self.stack.last() {
                        if !cond{
                            self.ip += *offset;
                        }
                    }
                },
                OpCode::Jump(offset) => {
                    self.ip += *offset;
                },
                OpCode::TmpPrint => {
                    println!("{:?}", self.stack.pop());
                }
                _ => unimplemented!()
            } 
            self.ip += 1;

        }

        Ok(())
    }
}

#[derive(thiserror::Error, Debug)]
pub enum VirtualMachineError{
    #[error("Generic error")]
    Generic,
    #[error("Invalid input for a binary op")]
    InvalidInpBinaryOp,
}

pub type VMResult<T> = Result<T, VirtualMachineError>;
