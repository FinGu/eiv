use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, ops::{Add, Div, Mul, Sub}, rc::Rc};
use std::fmt::{Debug, Display};

use enum_as_inner::EnumAsInner;

use crate::{compiler::{Function, StructDef, StructInst}, prelude::Callable};

#[derive(Debug)]
pub struct CallFrame{
    function: Rc<Function>,

    base: i32,

    ip: i32,
}

impl CallFrame{
    pub fn new(function: Rc<Function>, ip: i32, base: i32) -> Self{
        Self{
            function,
            ip, 
            base,
        }
    }
}

#[derive(Clone, Debug, EnumAsInner)]
pub enum Immediate{
    Number(f64),
    Char(u8),
    Boolean(bool),
    Array(Vec<Immediate>),
    Function(Rc<Function>),
    GlobalFunction(Box<dyn Callable>),
    StructDef(Rc<StructDef>),
    StructInst(Rc<RefCell<StructInst>>),
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
            _ => Self::Number({
                f64::NAN
            }) 
        }
    }
}

impl Sub for Immediate{
    type Output = Immediate;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs){
            (Self::Number(l), Self::Number(r)) => Self::Number(l - r),
            _ => Self::Number({
                f64::NAN
            })
        }
    }
}

impl Div for Immediate{
    type Output = Immediate;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs){
            (Self::Number(l), Self::Number(r)) => Self::Number(l / r),
            _ => Self::Number({

                f64::NAN
            })
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
            _ => Self::Number({
                f64::NAN
            })

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

    And,
    Or,

    BoolCast,
    NumberCast,
    CharCast,

    JumpIfFalse(i32),

    Jump(i32),

    SetLocal(i32),
    GetLocal(i32),

    GetGlobal(String),
    SetGlobal(String),

    GetProp(String),
    SetProp(String, Box<OpCode>),

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
            call_frames: Vec::new(),
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
                },
                OpCode::Less => {
                    (left < right).into()
                },
                OpCode::LessEqual =>{
                    (left <= right).into()
                },
                OpCode::Greater => {
                    (left > right).into()
                },
                OpCode::GreaterEqual => {
                    (left >= right).into()
                },
                OpCode::Or => {
                    match (left, right) {
                        (Immediate::Boolean(l), Immediate::Boolean(r)) => Immediate::Boolean(l || r),
                        _ => return Err(VirtualMachineError::InvalidInpBinaryOp),
                    }
                },
                OpCode::And => {
                    match (left, right) {
                        (Immediate::Boolean(l), Immediate::Boolean(r)) => Immediate::Boolean(l && r),
                        _ => return Err(VirtualMachineError::InvalidInpBinaryOp),
                    }

                }
                _ => {
                    return Err(VirtualMachineError::InvalidInpBinaryOp);
                }
            })
    }

    fn call_function(&mut self, params_num: usize) -> VMResult<()>{
        let base = self.stack.len() - params_num - 1;

        let func = &self.stack[base];

        match func{
            Immediate::Function(normal_func) => {
                if normal_func.arity != params_num{
                    return Err(VirtualMachineError::WrongNumParams);

                }
             
                let new_frame = CallFrame::new(Rc::clone(normal_func), 0, 
                            base as i32);

                self.call_frames.push(new_frame);
            },
            Immediate::GlobalFunction(global_func) => {
                global_func.clone().call(self, params_num)?;

                self.inc_ip(1); //global funcs won't do this for us
            },
            Immediate::StructDef(sdef) => {
                let new_inst = StructInst::new(sdef.clone());

                self.stack[base] = Immediate::StructInst(RefCell::new(new_inst).into());
            },
            Immediate::StructInst(inst) =>{
                self.inc_ip(1);
            },
            _ => {
                return Err(VirtualMachineError::FuncDoesntExist);
            }
        }

        Ok(())
    }
    
    fn get_cur_code(&self) -> &Vec<OpCode>{
        &self.get_cur_function().code
    }

    fn get_cur_function(&self) -> &Function{
        &self.get_cur_call_frame().function
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

    pub fn work(&mut self, function: Rc<Function>) -> VMResult<()>{
        self.call_frames.push(
            CallFrame::new(function.clone(), 0, 0)
        );

        self.stack.push(Immediate::Function(function.clone()));

        println!("{:?}", self.get_cur_code());

        while self.get_ip() < self.get_cur_code().len() as i32 {
            let el = self.get_cur_code()[self.get_ip() as usize].clone();

            //println!("IP: {}, Executing: {:?} with last stack value: {:?}", self.get_ip(), el, self.stack.last());

            match el{
                OpCode::Pop => {
                    self.stack.pop();
                },
                OpCode::Return =>{
                    let popped_return = self.stack.pop();
                                        
                    self.stack.truncate(self.get_cur_call_frame().base as usize);

                    self.call_frames.pop();

                    if self.call_frames.is_empty(){
                        self.stack.pop();

                        return Ok(());
                    }
                    
                    if let Some(ret) = popped_return{
                        self.stack.push(ret);
                    }
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
                OpCode::Equal | OpCode::NotEqual | OpCode::Less | OpCode::LessEqual | OpCode::Greater | OpCode::GreaterEqual | OpCode::And | OpCode::Or
                    =>{
                    let result = self.binary_op(&el);

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
                    let local = self.get_local(pos)?;

                    self.stack.push(local);
                },
                OpCode::SetLocal(pos) =>{
                    let last = self.stack.last().unwrap();

                    self.set_local(pos, last.clone());
                },
                OpCode::GetGlobal(name) => {
                    let global = self.globals.get(&name).cloned().unwrap_or(Immediate::Null);

                    self.stack.push(global);
                },
                OpCode::SetGlobal(name) => {
                    let last = self.stack.pop().unwrap();

                    self.globals.insert(name, last);
                },
                OpCode::GetProp(name) => {
                    let last = self.stack.pop().unwrap();

                    match last{
                        Immediate::StructInst(ref sinst) => {
                            let field = sinst.borrow().get_compiled_var(&name);

                            let data = match field{
                                None => sinst.borrow().dyn_data.get(&name).cloned().unwrap_or(Immediate::Null),
                                Some(field) => self.get_local(field)?
                            };

                            self.stack.push(data);
                        },
                        _ => return Err(VirtualMachineError::GetNotInstance)
                    }
                },
                OpCode::SetProp(name, op) => {
                    let rvalue = self.stack.pop().unwrap();
                    let mut inst = self.stack.pop().unwrap();

                    match inst{
                        Immediate::StructInst(ref mut sinst) => {
                            let old_pos = sinst.borrow().get_compiled_var(&name);

                            match old_pos{
                                None => {
                                    sinst.as_ref().borrow_mut().dyn_data.insert(name, rvalue);
                                },
                                Some(pos) => {
                                    let old_value = self.get_local(pos)?;

                                    self.set_local(pos, match *op{
                                        OpCode::Add => {
                                            old_value + rvalue
                                        },
                                        OpCode::Multiply => {
                                            old_value * rvalue
                                        },
                                        OpCode::Divide => {
                                            old_value / rvalue
                                        },
                                        OpCode::Subtract => {
                                            old_value - rvalue
                                        },
                                        _ => rvalue
                                    });

                                } 
                            }

                        },
                        _ => return Err(VirtualMachineError::GetNotInstance)
                    }
                },
                OpCode::JumpIfFalse(offset) => {
                    if let Some(Immediate::Boolean(cond)) = self.stack.last() {
                        if !cond{
                            self.inc_ip(offset);
                            continue;
                        }
                    }
                },
                OpCode::Jump(offset) => {
                    self.inc_ip(offset);
                    continue;
                },
                OpCode::Call(params_num) => {
                    self.call_function(params_num as usize)?;
                    continue;
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
    #[error("Wrong num of parameters to a func")]
    WrongNumParams,
    #[error("Get operation used on something that's not an instance")]
    GetNotInstance,
}

pub type VMResult<T> = Result<T, VirtualMachineError>;
