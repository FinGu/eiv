use std::fs::File;

use bincode::ErrorKind;

use crate::{
    ast::Statement, compiler::{Compiler, CompilerError, Function, SymbolTable}, errors, lexer::Lexer, parser::Parser, vm::{Immediate, OpCode, VirtualMachine, VirtualMachineError}
};

#[derive(thiserror::Error, Debug)]
pub enum EivError{
    #[error("Invalid file path")]
    InvalidFilePath,
    #[error("Error while lexing")]
    Lexing,
    #[error("Error while parsing")]
    Parsing,
    #[error("Compiler error")]
    Compiling(#[from] CompilerError),
    #[error("Serializer error")]
    Serializing(#[from] ErrorKind),
    #[error("Error while running the program: {0}")]
    VM(VirtualMachineError),
}

pub type EivResult<T> = Result<T, EivError>;

pub fn lex_and_parse(file_name: &str, file_contents: String) -> EivResult<Vec<Statement>>{
    let mut scanner = Lexer::new(file_contents);

    let tokens = scanner.work();

    if errors::LIST.lock().unwrap().report() {
        return Err(EivError::Lexing);
    }

    let mut parser = Parser::new(tokens, file_name);

    let statements = parser.work();

    if errors::LIST.lock().unwrap().report() {
        return Err(EivError::Parsing);
    }

    Ok(statements)
}

pub fn compile(symbol_table: SymbolTable, statements: Vec<Statement>, repl_mode: bool) -> EivResult<(Function, SymbolTable)>{
    let mut comp = Compiler::new(symbol_table, repl_mode);

    Ok((comp.work(statements)?, comp.symbol_table))
}

pub fn run_compiler(
    symbol_table: SymbolTable,
    file_contents: String,
    file_name: &str,
    embed_mode: bool,
    output: Option<String>
) -> EivResult<()>{
    let statements = lex_and_parse(file_name, file_contents)?;

    let (function, _) = compile(symbol_table, statements, false)?;

    let code = function.code;

    let result = bincode::serialize(&code)
        .map_err(|e| EivError::Serializing(*e))?;

    std::fs::write(match output{
        Some(path) => path,
        None => "out.vie".into()
    }, result)
    .expect("Failed to write to the output");

    Ok(())
}

pub fn run_bytecode(
    vm: &mut VirtualMachine,
    file_contents: &[u8],
) -> EivResult<Immediate> {
    let bytecode: Vec<OpCode> = bincode::deserialize(file_contents)
        .expect("Failed to deserialize the input");

    let function = Function{
        name: String::from(""),
        arity: 0,
        code: bytecode
    };

    let ret = vm.work(Some(function.into()))
        .map_err(EivError::VM)?;

    Ok(ret)
}

pub fn run_interpreter(
    symbol_table: SymbolTable,
    vm: &mut VirtualMachine,
    file_contents: String,
    file_name: &str,
    repl_mode: bool,
) -> EivResult<(Immediate, SymbolTable)> {
    let statements = lex_and_parse(file_name, file_contents)?;

    let (function, st) = compile(symbol_table, statements, repl_mode)?;

    let ret = vm.work(Some(function.into()))
        .map_err(EivError::VM)?;

    Ok((ret, st))
}
