use crate::{compiler::{Compiler, SymbolTable}, errors, lexer::Lexer, parser::Parser, vm::{OpCode, VirtualMachine, VirtualMachineError}};

pub fn run_interpreter(symbol_table: SymbolTable, vm: &mut VirtualMachine, _str: String, file_name: String) -> SymbolTable {
    let mut scanner = Lexer::new(_str);

    let tokens = scanner.work();

    errors::LIST.lock().unwrap().report();

    let mut parser = Parser::new(tokens, file_name);

    let statements = parser.work();
    
    errors::LIST.lock().unwrap().report();

    let mut comp = Compiler::new(symbol_table); 

    let result = comp.work(statements).unwrap();

    match vm.work(result.into()){
        Ok(_) => {},
        Err(e) => panic!("{}", e),
        
    }

    comp.symbol_table
}
