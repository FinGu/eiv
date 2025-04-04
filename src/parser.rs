use crate::{
    ast::{
        ArrayExpr, ArrayGetExpr, ArraySetStmt, BinaryExpr, CallExpr, CastExpr, ControlFlowType,
        CtrlStmt, ElseStmt, Expression, FnStmt, ForStmt, GetExpr, GroupingExpr, IfStmt,
        LiteralExpr, SetStmt, Statement, StructStmt, ThisExpr, UnaryExpr, VarExpr,
        VarStmt, WhileStmt,
    },
    errors,
    lexer::{Token, TokenType},
    vm::Immediate,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected token located while parsing")]
    UnexpectedToken,
    #[error("Bad expression during parsing")]
    BadExpression,
    #[error("Break/Continue outside of a loop")]
    BreakContOutsideLoop,
    #[error("Return outside of a function")]
    ReturnOutsideFn,

    #[error("Include outside of the main scope")]
    IncludeOutsideMainScope,
    #[error("File can't include itself")]
    CantIncludeItself,
    #[error("Expected a string to include")]
    ExpectedStringInclude,

    #[error("Bad get expression")]
    BadGetExpr,
}

pub struct Parser {
    tokens: Vec<Token>,

    cur: usize,
    loop_count: usize,
    fn_count: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,

            cur: 0,
            loop_count: 0,
            fn_count: 0,
        }
    }

    pub fn in_loop(&self) -> bool {
        self.loop_count > 0
    }

    pub fn in_fn(&self) -> bool {
        self.fn_count > 0
    }

    pub fn work(&mut self) -> Vec<Statement> {
        let mut stmts: Vec<Statement> = vec![];

        while !self.done() {
            if let Some(decl) = self.get_declaration(true) {
                stmts.push(decl);
            }
        }

        stmts
    }

    fn done(&self) -> bool {
        self.tokens[self.cur].token_type == TokenType::EOF
    }

    fn next(&mut self) -> &Token {
        let out = &self.tokens[self.cur];

        self.cur += 1;

        out
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.cur]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.cur - 1]
    }

    fn match_tokens(&mut self, types: &[TokenType]) -> bool {
        if self.done() {
            return false;
        }

        for token_type in types {
            if self.peek().token_type != *token_type {
                continue;
            }

            self.cur += 1;

            return true;
        }

        false
    }

    fn expect_next(&mut self, token_type: TokenType, save_errors: bool) -> Option<&Token> {
        let cur_tok = self.peek();

        if cur_tok.token_type == token_type {
            return Some(self.next());
        }

        if !save_errors {
            return None;
        }

        println!(
            "token expected: {:?}, token got: {:?}",
            token_type, cur_tok.token_type
        );

        errors::LIST
            .lock()
            .unwrap()
            .push(ParserError::UnexpectedToken, Some(cur_tok.clone()));

        None
    }

    pub fn get_arr_primary(&mut self, display_errors: bool) -> Expression {
        let mut exprs = Vec::new();

        if self.match_tokens(&[TokenType::RightBracket]) {
            //empty array

            return ArrayExpr::new(
                exprs,
                if self.match_tokens(&[TokenType::Pipe]) {
                    Some(self.next().clone())
                } else {
                    None
                },
            )
            .into();
        }

        exprs.push(self.get_expression(display_errors));

        while self.match_tokens(&[TokenType::Comma]) {
            exprs.push(self.get_expression(display_errors));
        }

        self.expect_next(TokenType::RightBracket, display_errors);

        ArrayExpr::new(
            exprs,
            if self.match_tokens(&[TokenType::Pipe]) {
                Some(self.next().clone())
            } else {
                None
            },
        )
        .into()
    }

    pub fn get_primary(&mut self, display_errors: bool) -> Option<Expression> {
        if self.match_tokens(&[TokenType::Null]) {
            return Some(Immediate::Null.into());
        }

        if self.match_tokens(&[TokenType::True]) {
            return Some(Immediate::Boolean(true).into());
        }

        if self.match_tokens(&[TokenType::False]) {
            return Some(Immediate::Boolean(false).into());
        }

        let cur_tok = self.peek().clone();

        if let TokenType::Char(chr) = cur_tok.token_type {
            self.cur += 1;

            return Some(Immediate::Char(chr).into());
        }

        if let TokenType::Number(num) = cur_tok.token_type {
            // we need to do this manually
            self.cur += 1;
            return Some(Immediate::Number(num).into());
        }

        if let TokenType::String(ref str_) = cur_tok.token_type {
            self.cur += 1;

            return Some(LiteralExpr::new(str_.clone().into_bytes().into()).into());
        }

        if self.match_tokens(&[TokenType::This]) {
            return Some(ThisExpr::new(self.previous().clone()).into());
        }

        if self.match_tokens(&[TokenType::LeftParen]) {
            let expr = self.get_expression(display_errors); // consumes the last expression

            //this is the expression contained between the parenthesis ^
            self.expect_next(TokenType::RightParen, display_errors);

            return Some(GroupingExpr::new(expr).into());
        }

        if self.match_tokens(&[TokenType::LeftBracket]) {
            return Some(self.get_arr_primary(display_errors));
        }

        if let TokenType::Identifier(_) = cur_tok.token_type {
            self.cur += 1;

            return Some(VarExpr::new(cur_tok).into());
        }

        if self.match_tokens(&[TokenType::EOS]) {
            while self.match_tokens(&[TokenType::EOS]) {}

            self.cur -= 1; // exhaust all the dangling end of statements

            return None;
        }

        if display_errors {
            errors::LIST
                .lock()
                .unwrap()
                .push(ParserError::BadExpression, Some(cur_tok));
        }

        self.cur += 1;
        //self.try_sync();

        None
    }

    //we need this display_errors thing to avoid a print in case of a bad attempt of parsing ( when
    //checking if it's trying to set a field)
    pub fn get_call(&mut self, display_errors: bool) -> Expression {
        let mut expr = match self.get_primary(display_errors) {
            Some(val) => val,
            None => return LiteralExpr::new(Immediate::Null).into(),
        };

        loop {
            if self.match_tokens(&[TokenType::LeftBracket]) {
                expr = ArrayGetExpr::new(expr, self.get_expression(display_errors)).into();

                self.expect_next(TokenType::RightBracket, display_errors);

                continue;
            }

            if self.match_tokens(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr);

                continue;
            }

            if self.match_tokens(&[
                TokenType::Dot,
                TokenType::DoubleColon,
                TokenType::LeftBracket,
            ]) {
                let cur = self.peek().clone();

                if let TokenType::Identifier(_) = cur.token_type {
                    let prev = self.previous().clone();

                    self.cur += 1;

                    expr =
                        GetExpr::new(expr, cur, prev.token_type == TokenType::DoubleColon).into();
                } else {
                    errors::LIST
                        .lock()
                        .unwrap()
                        .push(ParserError::BadGetExpr, Some(cur));
                }

                continue;
            }

            break;
        }

        expr
    }

    pub fn finish_call(&mut self, caller: Expression) -> Expression {
        let mut args: Vec<Expression> = Vec::new();

        if self.peek().token_type != TokenType::RightParen {
            args.push(self.get_expression(true));

            while self.match_tokens(&[TokenType::Comma]) {
                args.push(self.get_expression(true));
            }
        }

        let paren = self.expect_next(TokenType::RightParen, true);

        CallExpr::new(caller, paren.cloned(), args).into()
    }

    pub fn get_cast(&mut self, display_errors: bool) -> Expression {
        let mut expr = self.get_call(display_errors);

        while self.match_tokens(&[
            TokenType::BoolCast,
            TokenType::CharCast,
            TokenType::NumberCast,
            TokenType::IndexCast,
        ]) {
            let prev = self.previous().clone();

            expr = CastExpr::new(prev, expr).into();
        }

        expr
    }

    pub fn get_unary(&mut self, display_errors: bool) -> Expression {
        if self.match_tokens(&[TokenType::Minus, TokenType::Not]) {
            let op = self.previous().clone();

            return UnaryExpr::new(op, self.get_unary(display_errors)).into();
        }

        self.get_cast(display_errors)
    }

    pub fn get_factor(&mut self, display_errors: bool) -> Expression {
        let mut expr = self.get_unary(display_errors);

        while self.match_tokens(&[TokenType::Slash, TokenType::Star, TokenType::Percentage]) {
            let op = self.previous().clone();

            let right = self.get_unary(display_errors);

            expr = BinaryExpr::new(expr, op, right).into();
        }

        expr
    }

    pub fn get_term(&mut self, display_errors: bool) -> Expression {
        let mut expr = self.get_factor(display_errors);

        while self.match_tokens(&[TokenType::Minus, TokenType::Plus]) {
            let op = self.previous().clone();

            let right = self.get_factor(display_errors);

            expr = BinaryExpr::new(expr, op, right).into();
        }

        expr
    }

    pub fn get_comparison(&mut self, display_errors: bool) -> Expression {
        let mut expr = self.get_term(display_errors);

        while self.match_tokens(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let op = self.previous().clone();

            let right = self.get_term(display_errors);

            expr = BinaryExpr::new(expr, op, right).into();
        }

        expr
    }

    pub fn get_equality(&mut self, display_errors: bool) -> Expression {
        let mut expr = self.get_comparison(display_errors);

        while self.match_tokens(&[TokenType::NotEqual, TokenType::EqualEqual]) {
            let op = self.previous().clone();

            let right = self.get_comparison(display_errors);

            expr = BinaryExpr::new(expr, op, right).into();
        }

        expr
    }

    pub fn get_expression(&mut self, display_errors: bool) -> Expression {
        let mut expr = self.get_equality(display_errors);

        while self.match_tokens(&[TokenType::And, TokenType::Or]) {
            let op = self.previous().clone();

            let right = self.get_equality(display_errors);

            expr = BinaryExpr::new(expr, op, right).into();
        }

        expr
    }

    //this is a standalone expression, if it's a null literalexpr, it means it's useless
    pub fn get_expression_statement(&mut self, eos_check: bool) -> Option<Statement> {
        let expr = self.get_expression(true);

        if eos_check {
            self.expect_next(TokenType::EOS, true);
        }

        if let Expression::Literal(ref literal) = expr {
            if literal.value.is_null() {
                return None;
            }
        }

        Some(Statement::Expression(Box::new(expr.into())))
    }

    fn get_block(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();

        while !self.done() && self.peek().token_type != TokenType::RightBrace {
            if let Some(statement) = self.get_declaration(true) {
                statements.push(statement);
            }
        }

        self.expect_next(TokenType::RightBrace, true);

        statements
    }

    pub fn get_if_statement(&mut self) -> IfStmt {
        let cond = self.get_expression(true);

        self.expect_next(TokenType::LeftBrace, true);

        let statements = self.get_block();

        let mut poss_else = None;

        if self.match_tokens(&[TokenType::Else]) {
            poss_else = Some(if self.match_tokens(&[TokenType::If]) {
                //to support if elses
                ElseStmt::ElseIf(Box::new(self.get_if_statement()))
            } else {
                self.expect_next(TokenType::LeftBrace, true);

                let block_stmt = (self.get_block(), false).into();

                ElseStmt::Else(Box::new(block_stmt))
            });
        }

        IfStmt::new(cond, statements, poss_else)
    }

    pub fn get_while_statement(&mut self) -> Statement {
        let cond = self.get_expression(true);

        self.expect_next(TokenType::LeftBrace, true);

        self.loop_count += 1;
        let statements = self.get_block();
        self.loop_count -= 1;

        WhileStmt::new(cond, statements, None).into()
    }

    pub fn get_for_statement(&mut self) -> Statement {
        let decl = match self.get_optional_either_declaration(true) {
            Some(statement) => Some(statement),
            None => {
                self.expect_next(TokenType::EOS, true);
                None
            }
        };

        let cond = match self.match_tokens(&[TokenType::EOS]) {
            true => None,
            false => {
                let val = Some(self.get_expression(true));

                self.expect_next(TokenType::EOS, true);

                val
            }
        };

        let incr = self.get_optional_either_declaration(false);

        self.expect_next(TokenType::LeftBrace, true);

        self.loop_count += 1;
        let for_block = self.get_block();
        self.loop_count -= 1;

        ForStmt::new(decl, cond, incr, for_block).into()
    }

    pub fn get_variable_statement(&mut self, identifier: Token) -> Statement {
        let expr = self.get_expression(true);

        Statement::Variable(Box::new(VarStmt::new(identifier, expr)))
    }

    pub fn get_block_statement(&mut self) -> Statement {
        //vec<stmt>, is_standalone bool
        let block_stmt = (self.get_block(), true).into();

        Statement::Block(Box::new(block_stmt))
    }

    pub fn get_break_statement(&mut self) -> Statement {
        if !self.in_loop() {
            errors::LIST
                .lock()
                .unwrap()
                .push(ParserError::BreakContOutsideLoop, Some(self.peek().clone()));
        }

        self.expect_next(TokenType::EOS, true);

        CtrlStmt::new(ControlFlowType::Break).into()
    }

    pub fn get_continue_statement(&mut self) -> Statement {
        if !self.in_loop() {
            errors::LIST
                .lock()
                .unwrap()
                .push(ParserError::BreakContOutsideLoop, Some(self.peek().clone()));
        }

        self.expect_next(TokenType::EOS, true);

        CtrlStmt::new(ControlFlowType::Continue).into()
    }

    pub fn get_return_statement(&mut self) -> Statement {
        if !self.in_fn() {
            errors::LIST
                .lock()
                .unwrap()
                .push(ParserError::ReturnOutsideFn, Some(self.peek().clone()));
        }

        let expr = self.get_expression(true);

        self.expect_next(TokenType::EOS, true);

        CtrlStmt::new(ControlFlowType::Return(expr)).into()
    }

    //this is syntax sugar for `if a { b }`
    pub fn get_do_statement(&mut self) -> Statement {
        let statement = self.get_declaration(false).unwrap();

        self.expect_next(TokenType::If, true);

        let expression = self.get_expression(true);

        IfStmt::new(expression, vec![statement], None).into()
    }

    pub fn get_statement(&mut self, eos_check: bool) -> Option<Statement> {
        if self.match_tokens(&[TokenType::If]) {
            return Some(self.get_if_statement().into()); // to make my life easier
        }

        if self.match_tokens(&[TokenType::While]) {
            return Some(self.get_while_statement());
        }

        if self.match_tokens(&[TokenType::For]) {
            return Some(self.get_for_statement());
        }

        if self.match_tokens(&[TokenType::Break]) {
            return Some(self.get_break_statement());
        }

        if self.match_tokens(&[TokenType::Continue]) {
            return Some(self.get_continue_statement());
        }

        if self.match_tokens(&[TokenType::Return]) {
            return Some(self.get_return_statement());
        }

        if self.match_tokens(&[TokenType::Do]) {
            return Some(self.get_do_statement());
        }

        if self.match_tokens(&[TokenType::LeftBrace]) {
            return Some(self.get_block_statement());
        }

        self.get_expression_statement(eos_check)
    }

    pub fn get_parameters(&mut self) -> Vec<String> {
        let mut toks = Vec::new();

        loop {
            let cur_tok = self.peek().clone();

            if self.match_tokens(&[TokenType::Comma]) {
                continue;
            }

            if let TokenType::Identifier(ref e) = cur_tok.token_type {
                self.cur += 1;
                toks.push(e.clone());
            }

            if self.match_tokens(&[TokenType::RightParen]) {
                break;
            }
        }

        toks
    }

    pub fn get_function_declaration(&mut self, name: Token) -> Statement {
        let params = self.get_parameters();

        self.expect_next(TokenType::LeftBrace, true);

        self.fn_count += 1; //inside a function
        let block = self.get_block();
        self.fn_count -= 1;

        Statement::Function(FnStmt::new(name, params, block).into())
    }

    pub fn get_struct_declaration(&mut self, name: Token) -> Statement {
        let mut var_vec: Vec<Statement> = Vec::new();

        while !self.done() && !self.peek().token_type.is_right_brace() {
            let is_static = self.match_tokens(&[TokenType::Static]);

            match self.get_optional_either_declaration(true) {
                Some(mut res) => {
                    if is_static {
                        match res {
                            Statement::Variable(ref mut v) => {
                                v.is_static = true;
                            }
                            Statement::Struct(ref mut s) => {
                                s.is_static = true;
                            }
                            Statement::Function(ref mut f) => {
                                f.is_static = true;
                            }
                            _ => {}
                        };
                    }

                    var_vec.push(res);
                }
                None => {
                    //errors::LIST.lock().unwrap().push(ParserError::BadStmtStruct, Some(self.peek().clone()));
                    self.cur += 1;
                }
            };
        }

        self.expect_next(TokenType::RightBrace, true);

        Statement::Struct(StructStmt::new(name, var_vec).into())
    }

    pub fn get_shorthand_declaration(
        &mut self,
        mut op: Token,
        name: Token,
        eos_check: bool,
    ) -> Statement {
        let expression = self.get_expression(true);

        op.token_type = match op.token_type {
            TokenType::PlusEqual => TokenType::Plus,
            TokenType::MinusEqual => TokenType::Minus,
            TokenType::SlashEqual => TokenType::Slash,
            TokenType::StarEqual => TokenType::Star,
            TokenType::PercentageEqual => TokenType::Percentage,
            _ => panic!(),
        };

        // desugars a += 1; to a = a + 1;
        let out_expr = BinaryExpr::new(VarExpr::new(name.clone()).into(), op, expression);

        if eos_check {
            //in case of loops, for i = 0; i < b; i += 1, we can't have end of
            //expressions
            self.expect_next(TokenType::EOS, true);
        }

        VarStmt::new(name, out_expr.into()).into()
    }

    pub fn get_normal_declaration(&mut self, name: Token, eos_check: bool) -> Statement {
        let statement = if self.match_tokens(&[TokenType::LeftParen]) {
            let saved_pos = self.cur;

            self.cur -= 1; // the reason we decrement is so that the parser gets the left paren
            let expr = self.get_expression(false);

            // this is the only case where we have to
            // disable errors for get_expression

            //we have to deal with the ambiguity of
            //a = (dfg) + something
            //a = (dfg){}

            if self.match_tokens(&[TokenType::EOS]) {
                let stmt = VarStmt::new(name, expr);

                return stmt.into();
            }

            self.cur = saved_pos;

            self.get_function_declaration(name)
        } else if self.match_tokens(&[TokenType::LeftBrace]) {
            self.get_struct_declaration(name)
        } else {
            self.get_variable_statement(name)
        };

        if eos_check {
            self.expect_next(TokenType::EOS, true);
        }

        statement
    }

    pub fn get_call_declaration(&mut self, eos_check: bool) -> Option<Statement> {
        let saved_pos = self.cur;

        let expr = self.get_call(false);

        if self.match_tokens(&[
            TokenType::Equal,
            TokenType::PlusEqual,
            TokenType::MinusEqual,
            TokenType::SlashEqual,
            TokenType::StarEqual,
            TokenType::PercentageEqual,
        ]) {
            let previous = self.previous().clone();

            if expr.is_get() {
                let get_expr = expr.as_get().unwrap();

                let rexpr = self.get_expression(true); // we don't let people do instance.method = () {}
                                                       // pretty annoying to backport now that i've
                                                       // seen the bug, might as well say it wasn't
                                                       // planned

                if eos_check {
                    self.expect_next(TokenType::EOS, true);
                }

                return Some(
                    SetStmt::new(
                        get_expr.callee.clone(),
                        previous,
                        get_expr.argument.clone(),
                        rexpr,
                    )
                    .into(),
                );
            } else if expr.is_array_get() {
                let arr_expr = expr.as_array_get().unwrap();

                let rexpr = self.get_expression(true);

                if eos_check {
                    self.expect_next(TokenType::EOS, true);
                }

                return Some(
                    ArraySetStmt::new(
                        arr_expr.callee.clone(),
                        previous,
                        arr_expr.argument.clone(),
                        rexpr,
                    )
                    .into(),
                );
            } else {
                self.cur = saved_pos;
            }
        } else {
            self.cur = saved_pos;
        }

        self.get_optional_either_declaration(eos_check)
    }

    pub fn get_optional_either_declaration(&mut self, eos_check: bool) -> Option<Statement> {
        let cur_tok = self.peek().clone();

        if let TokenType::Identifier(_) = cur_tok.token_type {
            self.cur += 1;

            if self.match_tokens(&[
                TokenType::PlusEqual,
                TokenType::MinusEqual,
                TokenType::SlashEqual,
                TokenType::StarEqual,
                TokenType::PercentageEqual,
            ]) {
                return Some(self.get_shorthand_declaration(
                    self.previous().clone(),
                    cur_tok,
                    eos_check,
                ));
            }

            if self.match_tokens(&[TokenType::Equal]) {
                return Some(self.get_normal_declaration(cur_tok, eos_check));
            }

            self.cur -= 1;
        }

        None
    }

    pub fn get_declaration(&mut self, eos_check: bool) -> Option<Statement> {
        if let Some(statement) = self.get_call_declaration(eos_check) {
            return Some(statement);
        }

        if let Some(statement) = self.get_statement(eos_check) {
            return Some(statement);
        }

        None
    }
}
