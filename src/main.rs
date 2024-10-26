use std::env;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Write;

mod ast;
mod errors;
mod lexer;
mod parser;
mod utils;
mod vars;

#[cfg(test)]
mod tests;

fn prompt_mode() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    print!("> ");
    let _ = stdout.flush();

    for line in stdin.lock().lines() {
        match line {
            Err(_) => return,
            Ok(mut s) => {
                if s == "exit" {
                    return;
                }

                s += "\n";

                utils::run_interpreter(s, "<interpreter>".to_string()); // fuck it
            }
        }

        print!("> ");
        let _ = stdout.flush();
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();

    if args.len() <= 1 {
        prompt_mode();
        return;
    }

    let name = &args[1];

    let file = fs::read_to_string(name).expect("Failure to read the file");

    utils::run_interpreter(file, name.to_string());
}
