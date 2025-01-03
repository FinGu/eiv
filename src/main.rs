use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

use compiler::SymbolTable;
use vm::VirtualMachine;

mod ast;
mod compiler;
mod errors;
mod lexer;
mod parser;
mod prelude;
mod utils;
mod vm;

#[cfg(test)]
mod tests;

fn main() {
    let symbol_table = SymbolTable::new();

    let mut vm = VirtualMachine::new();

    prelude::include(&mut vm);

    let args: Vec<_> = env::args().collect();

    if args.len() <= 1 {
        return;
    }

    let name = &args[1];

    let file = fs::read_to_string(name).expect("Failure to read the file");

    utils::run_interpreter(symbol_table, &mut vm, file, name.to_string());
}
