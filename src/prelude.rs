use crate::{compiler::SymbolTable, vm::{CallFrame, Callable, Immediate, VMResult, VirtualMachine}};

#[derive(Debug, Clone)]
pub struct Print;

impl Callable for Print{
    fn name(&self) -> String {
        "print".to_owned()
    }

    fn arity(&self) -> usize {
       1 
    }

    fn call(&self, vm: &mut VirtualMachine, params_len: usize) -> VMResult<Immediate> {
        for _ in 0..params_len{
            print!("{:?}", vm.stack.pop().unwrap());
        } 
        Ok(Immediate::Null)
    } 

    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(self.clone()) 
    }
}

#[derive(Debug, Clone)]
pub struct PrintLn;

impl Callable for PrintLn{
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

    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(self.clone()) 
    }
}


fn insert(vm: &mut VirtualMachine, callable: Box<dyn Callable>){
    vm.globals.insert(callable.name(), Immediate::GlobalFunction(callable));
}

pub fn include(vm: &mut VirtualMachine){
    let gfuncs: Vec<Box<dyn Callable>> = vec![Box::new(Print), Box::new(PrintLn)];

    gfuncs.into_iter().for_each(|each| insert(vm, each));
}
