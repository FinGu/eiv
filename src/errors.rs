use std::{
    error::Error,
    fmt::Display,
    sync::{LazyLock, Mutex},
};

use crate::{lexer::Token, vm::Immediate};

pub struct ErrorTuple(Box<dyn Error + Sync + Send + 'static>, Option<Token>);

pub struct ErrorHandler {
    pub list: Vec<ErrorTuple>,
}

impl ErrorHandler {
    pub fn new() -> Self {
        Self { list: vec![] }
    }

    pub fn push<E: Error + Display + Sync + Send + 'static>(
        &mut self,
        e: E,
        token: Option<Token>,
    ) -> Immediate {
        self.list.push(ErrorTuple(Box::new(e), token));
        Immediate::Null
    }

    pub fn report(&mut self) -> bool {
        let mut had_errors = false;

        for each in self.list.iter() {
            had_errors = true;

            print!("Error detected: {}", each.0);

            if let Some(token) = &each.1 {
                println!(" at line {}", token.line);
            } else {
                println!();
            }
        }

        self.list.clear();

        had_errors
    }
}

pub static LIST: LazyLock<Mutex<ErrorHandler>> = LazyLock::new(|| Mutex::new(ErrorHandler::new()));
