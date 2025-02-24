use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;
use clap::{Parser, Subcommand};

use eiv::compiler::SymbolTable;
use eiv::prelude;
use eiv::utils;
use eiv::utils::{EivResult, EivError};
use eiv::vm::Immediate;
use eiv::vm::VirtualMachine;

#[derive(Parser)]
#[command(version)]
struct Args{
    #[command(subcommand)]
    cmds: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands{
    Run{
        path: String,
        #[arg(short, long)]
        bytecode: bool
    },
    Compile{
        path: String,
        #[arg(short, long)]
        output: Option<String>,
        #[arg(short, long)]
        embed: bool,
    },
}

fn prompt_mode(vm: &mut VirtualMachine) -> EivResult<()> {
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

fn main() -> EivResult<()> {
    let mut vm = VirtualMachine::new();

    prelude::include(&mut vm);

    let cmds = Args::parse().cmds;

    match cmds{
        None => {
            while prompt_mode(&mut vm).is_err() {}
            return Ok(());
        },
        Some(cmds) => {
            let symbol_table = SymbolTable::new();

            match cmds{
                Commands::Run { path, bytecode } => {
                    if bytecode{
                        let file = std::fs::read(path)
                            .map_err(|_| EivError::InvalidFilePath)?;

                        utils::run_bytecode(&mut vm, &file)?;

                        return Ok(());
                    } 

                    let file = fs::read_to_string(&path)
                        .map_err(|_| EivError::InvalidFilePath)?;

                    utils::run_interpreter(symbol_table, &mut vm, file, &path, false)?;
                },
                Commands::Compile { path, output, embed } => {
                    let file = fs::read_to_string(&path)
                        .map_err(|_| EivError::InvalidFilePath)?;

                    utils::run_compiler(symbol_table, file, &path, embed, output)?;
                }
            }
        }
    }

    Ok(())
}
