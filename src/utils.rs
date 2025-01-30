use crate::{
    compiler::{Compiler, SymbolTable},
    errors,
    lexer::Lexer,
    parser::Parser,
    vm::{Immediate, VirtualMachine, VirtualMachineError},
};

#[derive(thiserror::Error, Debug)]
pub enum InterpreterError {
    #[error("Error while lexing")]
    Lexing,
    #[error("Error while parsing")]
    Parsing,
    #[error("Error while running the program: {0}")]
    VM(VirtualMachineError),
}

pub type InterpreterResult<T> = Result<T, InterpreterError>;

pub fn run_interpreter(
    symbol_table: SymbolTable,
    vm: &mut VirtualMachine,
    file_contents: String,
    file_name: &str,
    repl_mode: bool,
) -> InterpreterResult<(Immediate, SymbolTable)> {
    let mut scanner = Lexer::new(file_contents);

    let tokens = scanner.work();

    if errors::LIST.lock().unwrap().report() {
        return Err(InterpreterError::Lexing);
    }

    let mut parser = Parser::new(tokens, file_name);

    let statements = parser.work();

    if errors::LIST.lock().unwrap().report() {
        return Err(InterpreterError::Parsing);
    }

    let mut comp = Compiler::new(symbol_table, repl_mode);

    let result = comp.work(statements).unwrap();

    let ret = vm.work(Some(result.into())).map_err(InterpreterError::VM)?;

    Ok((ret, comp.symbol_table))
}
