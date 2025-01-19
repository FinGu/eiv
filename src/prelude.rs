use std::{fmt::Debug, io::Write, rc::Rc};

use crate::{prelude, vm::{Immediate, VMResult, VirtualMachine, VirtualMachineError}};

pub trait Callable: Sync + Send {
    fn arity(&self) -> usize;

    fn call(&self, vm: &mut VirtualMachine, params: usize) -> VMResult<Immediate>;

    fn name(&self) -> String;
}

impl Debug for dyn Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Callable").finish()
    }
}

#[derive(Debug, Clone)]
pub struct Print;

impl Callable for Print {
    fn name(&self) -> String {
        "print".to_owned()
    }

    fn arity(&self) -> usize { // we don't care
        0
    }

    fn call(&self, vm: &mut VirtualMachine, params_len: usize) -> VMResult<Immediate> {
        let len = vm.stack.len();
        let window = &vm.stack[len - params_len..]; 

        for value in window.iter().take(params_len){
            if let Immediate::StructInst(ref inst) = value{
                let display = inst.as_ref().borrow().get("_display_");

                if display == Immediate::Null || !display.is_function(){
                    print!("{}", value);
                    continue;
                } 

                let ufunc = display.as_function().unwrap().clone();

                vm.standalone_work(ufunc, value.clone(), None)?;

                continue;
            }

            print!("{}", value);
        }
        
        Ok(Immediate::Null)
    }
}

#[derive(Debug, Clone)]
pub struct PrintLn;

impl Callable for PrintLn {
    fn name(&self) -> String {
        "println".to_owned()
    }

    fn arity(&self) -> usize {
        1
    }

    fn call(&self, vm: &mut VirtualMachine, params_len: usize) -> VMResult<Immediate> {
        let pr = Print;

        pr.call(vm, params_len)?;

        println!();

        Ok(Immediate::Null)
    }
}

#[derive(Debug, Clone)]
pub struct _TypeOf;

impl Callable for _TypeOf{
    fn call(&self, vm: &mut VirtualMachine, _: usize) -> VMResult<Immediate> {
        let arg = vm.stack.last().unwrap();

        let result = match arg{
            Immediate::Null => "null",
            Immediate::Char(_) => "char",
            Immediate::Array(_) => "array",
            Immediate::Number(_) => "number",
            Immediate::Boolean(_) => "bool",
            Immediate::Function(_) | Immediate::BoundFunction(_) => "function",
            Immediate::GlobalFunction(_) => "global_function",
            Immediate::StructDef(_) => "struct_definition",
            Immediate::StructInst(_) => "struct_instance",
        };

        let mut out_vec = Vec::new();

        result.chars().for_each(|e| out_vec.push(Immediate::Char(e as u8)));

        Ok(out_vec.into())
    }

    fn name(&self) -> String {
        "_typeof".into()
    }

    fn arity(&self) -> usize {
        1
    }
}

#[derive(Debug, Clone)]
pub struct TypeOf;

impl Callable for TypeOf{
    fn call(&self, vm: &mut VirtualMachine, params: usize) -> VMResult<Immediate> {
        let arg = vm.stack.last().unwrap();
        
        if let Immediate::StructInst(instance) = arg{
            let uinstance = instance.as_ref().borrow();

            if let Some(kind) = uinstance.data.get("_type_"){
                return Ok(kind.clone());
            }
        }

        let tpof = _TypeOf;

        tpof.call(vm, params)
    }

    fn name(&self) -> String {
        "typeof".into()
    }

    fn arity(&self) -> usize {
        1
    }
}

fn insert(vm: &mut VirtualMachine, callable: Rc<dyn Callable>) {
    vm.globals
        .insert(callable.name(), Immediate::GlobalFunction(callable));
}

pub fn include_from(dest_vm: &mut VirtualMachine, source_vm: &VirtualMachine){
    source_vm.globals.iter().for_each(|(name, value)| {
        dest_vm.globals.insert(name.clone(), value.clone());
    });
}

pub fn include(vm: &mut VirtualMachine) {
    let gfuncs: Vec<Rc<dyn Callable>> = vec![Rc::new(Print), Rc::new(PrintLn), Rc::new(_TypeOf), Rc::new(TypeOf)];

    gfuncs.into_iter().for_each(|each| insert(vm, each));
}
