use crate::{compiler::Compiler, errors, lexer::Lexer, parser::Parser, vm::{OpCode, VirtualMachine}};

pub fn run_interpreter(_str: String, file_name: String) {
    let mut scanner = Lexer::new(_str);

    let tokens = scanner.work();

    errors::LIST.lock().unwrap().report();

    let mut parser = Parser::new(tokens, file_name);

    let statements = parser.work();
    
    errors::LIST.lock().unwrap().report();

    let mut comp = Compiler::new(); 

    let mut result = comp.work(statements);

    result.push(OpCode::Return);

    let mut vm = VirtualMachine::new();

    println!("{:?}", result);

    let _ = vm.work(&result);
}
