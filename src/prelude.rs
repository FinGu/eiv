use std::{fmt::Debug, io::Write, rc::Rc};

use crate::{prelude, vm::{Immediate, VMResult, VirtualMachine, VirtualMachineError}};

pub trait Callable: Sync + Send {
    fn arity(&self) -> usize;

    fn clone_box(&self) -> Box<dyn Callable>;

    fn call(&self, vm: &mut VirtualMachine, params: usize) -> VMResult<()>;

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

#[derive(Debug, Clone)]
pub struct Print;

impl Callable for Print {
    fn name(&self) -> String {
        "print".to_owned()
    }

    fn arity(&self) -> usize {
        1
    }

    fn call(&self, vm: &mut VirtualMachine, params_len: usize) -> VMResult<()> {
        for _ in 0..params_len {
            let value = vm.stack.pop().unwrap();

            if let Immediate::StructInst(ref inst) = value{
                let display = inst.as_ref().borrow().get("_display_");

                if display == Immediate::Null || !display.is_function(){
                    print!("{}", Immediate::Null);
                    continue;
                } 

                let mut temp_vm = VirtualMachine::new();

                prelude::include(&mut temp_vm); 
                // this is so inefficient it's not even funny

                let ufunc = display.as_function().unwrap().clone();

                temp_vm.setup_call_frame(ufunc);

                temp_vm.call_frames
                    .last_mut()
                    .unwrap()
                    .instance = Some(value);

                temp_vm.work(None)?;

                continue;
            }

            print!("{}", value);
        }

        Ok(())
    }

    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(self.clone())
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

    fn call(&self, vm: &mut VirtualMachine, params_len: usize) -> VMResult<()> {
        let pr = Print;

        pr.call(vm, params_len)?;

        println!();

        Ok(())
    }

    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(self.clone())
    }
}

fn insert(vm: &mut VirtualMachine, callable: Rc<dyn Callable>) {
    vm.globals
        .insert(callable.name(), Immediate::GlobalFunction(callable));
}

pub fn include(vm: &mut VirtualMachine) {
    let gfuncs: Vec<Rc<dyn Callable>> = vec![Rc::new(Print), Rc::new(PrintLn)];

    gfuncs.into_iter().for_each(|each| insert(vm, each));
}
