use std::{fmt::Debug, io::Write};

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

                let ufunc = display.as_function().unwrap().clone();

                temp_vm.setup_call_frame(ufunc);

                let len = temp_vm.call_frames.len();
                temp_vm.call_frames[len-1].instance = Some(value);

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

fn insert(vm: &mut VirtualMachine, callable: Box<dyn Callable>) {
    vm.globals
        .insert(callable.name(), Immediate::GlobalFunction(callable));
}

pub fn include(vm: &mut VirtualMachine) {
    let gfuncs: Vec<Box<dyn Callable>> = vec![Box::new(Print), Box::new(PrintLn)];

    gfuncs.into_iter().for_each(|each| insert(vm, each));
}
