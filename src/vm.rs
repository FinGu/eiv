use std::{cell::RefCell, cmp::Ordering, collections::HashMap, ops::{Add, Div, Mul, Sub}, rc::Rc};
use std::fmt::{Debug, Display};

use enum_as_inner::EnumAsInner;

use crate::compiler::Function;

pub trait Callable: Sync + Send {
    fn arity(&self) -> usize;

    fn clone_box(&self) -> Box<dyn Callable>;

    fn call(&self, vm: &mut VirtualMachine, params: usize) -> VMResult<Immediate>;

    fn name(&self) -> String;
}

impl Debug for dyn Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Callable").finish()
    }
}

impl Clone for Box<dyn Callable> {
    fn clone(&self) -> Box<dyn Callable> {
        self.clone_box()
    }
}

#[derive(Debug)]
pub struct CallFrame{
    base: i32,

    ip: i32
}

impl Callable for Function{
    fn arity(&self) -> usize {
        self.arity

    }

    fn name(&self) -> String {
        self.name.clone()
    }

    fn clone_box(&self) -> Box<dyn Callable> {
       Box::new(self.clone()) 
    }

    fn call(&self, vm: &mut VirtualMachine, params_len: usize) -> VMResult<Immediate> {
        if self.arity() != params_len{
            return Err(VirtualMachineError::BadArity);
        }

        let new_frame = CallFrame::new(0, (vm.stack.len() - params_len) as i32);

        vm.call_frames.push(new_frame);

        vm.work(self)?;

        Ok(Immediate::Null)
    }
}

impl CallFrame{
    pub fn new(ip: i32, base: i32) -> Self{
        Self{
            ip, 
            base 
        }
    }
}


#[derive(Clone, Debug, EnumAsInner)]
pub enum Immediate{
    Number(f64),
    Char(u8),
    Boolean(bool),
    Array(Vec<Immediate>),
    Function(Function),
    GlobalFunction(Box<dyn Callable>),
    Null,
}

impl Display for Immediate{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self{
            Self::Number(num) => write!(f, "{}", num),
            Self::Char(chr) => write!(f, "{}", chr),
            Self::Null => write!(f, "null"),
            _ => {
                unimplemented!()
            }
        }
        
    }
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
            _ => None
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

#[derive(Debug, Clone, EnumAsInner)]
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

    SetLocal(i32),
    GetLocal(i32),
    GetGlobal(String),

    StartFn(i32),

    Call(i32),

    Constant(Immediate),

    Nop
}

pub struct VirtualMachine{
    pub globals: HashMap<String, Immediate>,
    pub call_frames: Vec<CallFrame>,
    pub stack: Vec<Immediate>,
}

impl VirtualMachine{
    pub fn new() -> Self{
        Self{
            stack: Vec::new(),
            call_frames: vec![CallFrame::new(0, 0)],
            globals: HashMap::new()
        }
    }

    fn get_local(&mut self, pos: i32) -> VMResult<Immediate> {
        let frame = self.call_frames.last().unwrap();

        let stack_index = frame.base + pos;

        Ok(self.stack.get(stack_index as usize).cloned()
            .unwrap_or(Immediate::Null))
    }

    fn set_local(&mut self, pos: i32, data: Immediate) {
        let frame = self.call_frames.last().unwrap();

        let stack_index = (frame.base + pos) as usize;

        if stack_index < self.stack.len() {
            self.stack[stack_index] = data;
        } else {
            self.stack.push(data);
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

    fn get_cur_call_frame(&self) -> &CallFrame{
        self.call_frames.last().unwrap()
    }

    fn set_ip(&mut self, new: i32){
        let target = self.call_frames.len() - 1;

        self.call_frames[target].ip = new;
    }

    fn get_ip(&self) -> i32{
        self.get_cur_call_frame().ip
    }

    fn inc_ip(&mut self, inc: i32) {
        let target = self.call_frames.len() - 1;

        self.call_frames[target].ip += inc;
    }

    pub fn work(&mut self, function: &Function) -> VMResult<()>{
        let instructions = &function.code;

        self.set_ip(0);

        let len = instructions.len() as i32;

        //println!("{:?}", instructions);

        while self.get_ip() < len{
            let el = &instructions[self.get_ip() as usize];

            match el{
                OpCode::Pop => {
                    self.stack.pop();
                },
                OpCode::Return =>{
                    let popped_return = self.stack.pop();

                    self.call_frames.pop();

                    if self.call_frames.is_empty(){
                        self.stack.pop();

                        return Ok(());
                    }

                    if let Some(ret) = popped_return{
                        self.stack.push(ret);
                    }

                    return Ok(());
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
                OpCode::GetLocal(pos) => {
                    let local = self.get_local(*pos)?;

                    self.stack.push(local);
                },
                OpCode::SetLocal(pos) =>{
                    let popped = self.stack.pop().unwrap();

                    self.set_local(*pos, popped.clone());
                },
                OpCode::GetGlobal(name) => {
                    let global = self.globals.get(name).cloned().unwrap_or(Immediate::Null);

                    self.stack.push(global);
                }
                OpCode::JumpIfFalse(offset) => {
                    if let Some(Immediate::Boolean(cond)) = self.stack.last() {
                        if !cond{
                            self.inc_ip(*offset);
                        }
                    }
                },
                OpCode::Jump(offset) => {
                    self.inc_ip(*offset);
                },
                OpCode::Call(params_num) => {
                    let params_num = *params_num as usize;
                    let popped = self.stack.pop().unwrap();

                    match popped{
                        Immediate::Function(func) => {
                            func.call(self, params_num)?;
                        },
                        Immediate::GlobalFunction(func) => {
                            func.call(self, params_num)?;
                        },
                        _ => return Err(VirtualMachineError::FuncDoesntExist)

                    };
                },
                OpCode::StartFn(name) => {
                    let popped = self.stack.pop().unwrap();

                    self.set_local(*name, popped);
                },
                OpCode::Nop => {}
                _ => unimplemented!()
            } 
            self.inc_ip(1);
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
    #[error("Invalid number of parameters")]
    BadArity,
    #[error("Function doesn't exist")]
    FuncDoesntExist,
}

pub type VMResult<T> = Result<T, VirtualMachineError>;