use std::fmt::Display;
use std::str::FromStr;

use enum_as_inner::EnumAsInner;
use thiserror::{self, Error};

use crate::errors;

#[derive(Error, Debug)]
enum LexError {
    #[error("Unterminated string in the lexer")]
    UnterminatedString,
    #[error("Unterminated char in the lexer")]
    UnterminatedChar,
    #[error("Bad number")]
    BadNumber,
}

type LexResult<T> = Result<T, LexError>;

#[derive(Clone, Debug, PartialEq, PartialOrd, EnumAsInner)]
#[allow(clippy::upper_case_acronyms)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    Comma,
    Dot,
    Minus,
    Plus,
    Slash,
    Star,
    Percentage,
    EOS,

    Not,
    NotEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Colon,
    DoubleColon,

    PlusEqual,
    MinusEqual,
    SlashEqual,
    StarEqual,
    PercentageEqual,

    Identifier(String),
    String(String),
    Number(f64),

    Char(u8),

    Ampersand,
    Pipe,
    And,
    Or,

    Else,
    False,
    True,
    For,
    If,
    Do,
    Null,
    Return,
    This,
    While,
    Break,
    Continue,
    Static,
    Include,

    As,
    BoolCast,
    NumberCast,
    CharCast,
    IndexCast,

    EOF,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl TokenType {
    pub fn try_from_two(
        current_idx: &mut usize,
        cur: char,
        next: Option<char>,
    ) -> Result<Self, ()> {
        let mut check_next = |tok, dual_tok_type, tok_type| {
            if let Some(val) = next {
                if val == tok {
                    // ew
                    *current_idx += 1;
                    dual_tok_type
                } else {
                    tok_type
                }
            } else {
                tok_type
            }
        };

        Ok(match cur {
            '!' => check_next('=', TokenType::NotEqual, TokenType::Not),
            '=' => check_next('=', TokenType::EqualEqual, TokenType::Equal),
            '<' => check_next('=', TokenType::LessEqual, TokenType::Less),
            '>' => check_next('=', TokenType::GreaterEqual, TokenType::Greater),
            '+' => check_next('=', TokenType::PlusEqual, TokenType::Plus),
            '-' => check_next('=', TokenType::MinusEqual, TokenType::Minus),
            '*' => check_next('=', TokenType::StarEqual, TokenType::Star),
            '%' => check_next('=', TokenType::PercentageEqual, TokenType::Percentage),
            '&' => check_next('&', TokenType::And, TokenType::Ampersand),
            '|' => check_next('|', TokenType::Or, TokenType::Pipe),
            ':' => check_next(':', TokenType::DoubleColon, TokenType::Colon),
            _ => return Err(()),
        })
    }
}

impl TryFrom<char> for TokenType {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(match value {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            '[' => TokenType::LeftBracket,
            ']' => TokenType::RightBracket,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            _ => return Err(()),
        })
    }
}

impl TryFrom<&str> for TokenType {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "true" => TokenType::True,
            "for" => TokenType::For,
            "if" => TokenType::If,
            "do" => TokenType::Do,
            "null" => TokenType::Null,
            "return" => TokenType::Return,
            "this" => TokenType::This,
            "while" => TokenType::While,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "stat" => TokenType::Static,
            "include" => TokenType::Include,

            "as" => TokenType::As,
            "number" => TokenType::NumberCast,
            "bool" => TokenType::BoolCast,
            "char" => TokenType::CharCast,
            "index" => TokenType::IndexCast,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: i32,
}

impl Token {
    pub fn new(kind: TokenType, line: i32) -> Token {
        Self {
            token_type: kind,
            line,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_type)
    }
}

pub struct Lexer {
    source: String,
    tokens: Vec<Token>,
    current_idx: usize,
    start_idx: usize,
    line: i32,
}

impl Lexer {
    pub fn new(source: String) -> Lexer {
        Self {
            source,
            tokens: Vec::new(),
            current_idx: 0,
            start_idx: 0,
            line: 1,
        }
    }

    fn is_useless(&mut self, c: char) -> bool {
        matches!(c, ' ' | '\r' | '\t')
    }

    fn push_token(&mut self, kind: TokenType) {
        self.tokens.push(Token::new(kind, self.line));
    }

    fn next(&mut self) -> char {
        let out = self
            .source
            .chars()
            .nth(self.current_idx)
            .expect("Unexpected indexing");

        self.current_idx += 1;

        out
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current_idx)
    }

    fn done(&self) -> bool {
        self.current_idx >= self.source.len()
    }

    fn get_comment(&mut self, multiline: bool) -> LexResult<()> {
        if multiline {
            while !self.done() {
                let chr = self.next();

                if chr == '*' {
                    if let Some(rl) = self.peek() {
                        if rl == '/' {
                            self.next();
                            break;
                        }
                    }
                }
            }
        } else {
            while !self.done() {
                if self.next() == '\n' {
                    self.line += 1;
                    break;
                }
            }
        }

        Ok(())
    }

    fn get_string(&mut self) -> LexResult<()> {
        let mut c;

        while !self.done() {
            c = self.peek().unwrap();

            if c == '"' {
                self.next();
                break;
            }

            if c == '\n' {
                self.line += 1;
            }

            self.next();
        }

        if self.done() {
            self.next();
            return Err(LexError::UnterminatedString);
        }

        self.push_token(TokenType::String(
            String::from(&self.source[self.start_idx + 1..self.current_idx - 1]), // could theoretically not allocate
        ));

        Ok(())
    }

    fn get_char(&mut self) -> LexResult<()> {
        let mut c = self.next();

        if c == '\\' {
            let next_part = self.next();

            c = match next_part {
                'n' => '\n',
                't' => '\t',
                _ => c,
            };
        }

        if self.done() || self.next() != '\'' {
            return Err(LexError::UnterminatedChar);
        }

        self.push_token(TokenType::Char(c as u8));

        Ok(())
    }

    fn get_number(&mut self) -> LexResult<()> {
        let mut dig_flag = false;
        let mut c;

        while !self.done() {
            c = self.peek().unwrap();

            if c.is_numeric() {
                self.next();
                continue;
            }

            if c != '.' {
                break;
            }

            if dig_flag {
                break;
            }

            dig_flag = true;

            self.next();

            if self.peek().is_none() || !self.peek().unwrap().is_numeric() {
                self.current_idx -= 1;
                break;
            }
        }

        let result = f64::from_str(&self.source[self.start_idx..self.current_idx])
            .map_err(|_| LexError::BadNumber)?;

        self.push_token(TokenType::Number(result));

        Ok(())
    }

    fn get_ident(&mut self) -> LexResult<()> {
        let mut c;

        while !self.done() {
            c = self.peek().unwrap();

            if !c.is_alphabetic() && !c.is_numeric() && c != '_' {
                break;
            }

            self.next();
        }

        let ident = &self.source[self.start_idx..self.current_idx];

        if let Ok(token_type) = TokenType::try_from(ident) {
            if token_type.is_as() {
                return Ok(());
            }

            self.push_token(token_type);
        } else {
            self.push_token(TokenType::Identifier(String::from(ident)));
        }

        Ok(())
    }

    fn scan(&mut self) -> LexResult<()> {
        self.start_idx = self.current_idx;

        let cur = self.next();

        if let Ok(token_type) = TokenType::try_from(cur) {
            self.push_token(token_type);
            return Ok(());
        }

        let opt_next = self.peek();

        if let Ok(token_type) = TokenType::try_from_two(&mut self.current_idx, cur, opt_next) {
            self.push_token(token_type);
            return Ok(());
        }

        if cur == '\n' {
            self.line += 1;

            self.push_token(TokenType::EOS);

            return Ok(());
        }

        if cur == ';' {
            self.push_token(TokenType::EOS);
            return Ok(());
        }

        if cur == '/' {
            if let Some(peeked) = self.peek() {
                match peeked {
                    '=' => {
                        self.push_token(TokenType::SlashEqual);
                        self.next();
                    }
                    '*' | '/' => {
                        return self.get_comment(peeked == '*');
                    }
                    _ => {
                        self.push_token(TokenType::Slash);
                    }
                }

                return Ok(());
            }

            //this should be an err
            self.push_token(TokenType::Slash);
            return Ok(());
        }

        if self.is_useless(cur) {
            return Ok(());
        }

        if cur == '"' {
            return self.get_string();
        }

        if cur == '\'' {
            return self.get_char();
        }

        if cur.is_numeric() {
            return self.get_number();
        }

        if cur.is_alphabetic() || cur == '_' {
            //special case
            return self.get_ident();
        }

        Ok(())
    }

    pub fn work(&mut self) -> Vec<Token> {
        while !self.done() {
            if let Err(e) = self.scan() {
                let last = self.tokens.last().cloned();

                errors::LIST.lock().unwrap().push(e, last);
            }
        }

        self.tokens.push(Token::new(TokenType::EOF, self.line));

        self.tokens.clone()
    }
}
