use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

use compiler::SymbolTable;
use utils::InterpreterResult;
use vm::Immediate;
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
fn prompt_mode(vm: &mut VirtualMachine) -> InterpreterResult<()> {
    let mut symbol_table = SymbolTable::new();

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    print!("> ");
    let _ = stdout.flush();

    for line in stdin.lock().lines() {
        match line {
            Err(_) => return Ok(()),
            Ok(mut s) => 'match_ok_block: {
                if s == "exit" {
                    return Ok(());
                }

                s += "\n";

                let (last_returned_value, used_st) =
                    utils::run_interpreter(symbol_table, vm, s, "<repl>", true)?;

                symbol_table = used_st;

                if last_returned_value.is_null() {
                    break 'match_ok_block;
                }

                if let Immediate::Function(ref fun) = last_returned_value {
                    if fun.name.is_empty() {
                        break 'match_ok_block;
                    }
                }

                println!("{}", last_returned_value);
            }
        }

        print!("> ");
        let _ = stdout.flush();
    }

    Ok(())
}

fn main() -> InterpreterResult<()> {
    let mut vm = VirtualMachine::new();

    prelude::include(&mut vm);

    let args: Vec<_> = env::args().collect();

    if args.len() <= 1 {
        while prompt_mode(&mut vm).is_err() {}
        return Ok(());
    }

    let name = &args[1];

    let file = fs::read_to_string(name).expect("Failure to read the file");

    utils::run_interpreter(SymbolTable::new(), &mut vm, file, name, false)?;

    Ok(())
}
