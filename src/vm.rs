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

    DefineGlobal(String),
    Constant(Immediate)
}

pub struct VirtualMachine{
    stack: Vec<Immediate>
}

impl VirtualMachine{
    pub fn new() -> Self{
        Self{
            stack: Vec::new()
        }
    }

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
        let mut globals: HashMap<&str, Immediate> = HashMap::new();

        for el in instructions{
            match el{
                OpCode::Pop => {
                    println!("{:?}",self.stack.pop());
                },
                OpCode::Return =>{
                    self.stack.pop();

                    println!("{:?}", globals);
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
                OpCode::Constant(c) =>{
                    self.stack.push(c.clone());
                },
                OpCode::DefineGlobal(name) =>{
                    globals.insert(name, self.stack.pop().unwrap());
                }
                _ => unimplemented!()
            } 
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
