use crate::{ast::Interpreter, errors, lexer::Lexer, parser::Parser};

pub fn run_interpreter(_str: String, file_name: String) {
    let mut scanner = Lexer::new(_str);

    let tokens = scanner.work();

    errors::LIST.lock().unwrap().report();

    let mut parser = Parser::new(tokens, file_name);

    let statements = parser.work();
    
    errors::LIST.lock().unwrap().report();

    let interpreter = Interpreter::new();

    interpreter.work(statements);

    errors::LIST.lock().unwrap().report();
}
