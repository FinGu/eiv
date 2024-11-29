use enum_as_inner::EnumAsInner;
use std::fmt::{Debug, Display};

use crate::{lexer::Token, vm::Immediate};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum AstError {
    #[error("Invalid use of the add operator")]
    InvalidUseOfAddOperator,
    #[error("Invalid use of the subtract operator")]
    InvalidUseOfSubtractOperator,
    #[error("Invalid use of the mul operator")]
    InvalidUseOfMulOperator,
    #[error("Invalid use of the modulus operator")]
    InvalidUseOfModOperator,
    #[error("Invalid use of div operator")]
    InvalidUseOfDivOperator,
    #[error("Invalid use of greater operator")]
    InvalidUseOfGreaterOperator,
    #[error("Invalid use of equal operator")]
    InvalidUseOfEqualOperator,
    #[error("Invalid use of less operator")]
    InvalidUseOfLessOperator,
    #[error("Invalid use of and operator")]
    InvalidUseOfAndOperator,
    #[error("Invalid use of or operator")]
    InvalidUseOfOrOperator,
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Unknown unary operator")]
    UnknownUnaryOperator,
    #[error("Unknown binary operator")]
    UnknownBinaryOperator,
    #[error("Can't negate anything other than a number")]
    NegationError,
    #[error("Can't 'not' anything other than numbers and bools")]
    NotError,
    #[error("Bad type to use in a cast")]
    BadCastingType,
    #[error("Bad conditional expression")]
    BadConditionalExpression,
    #[error("Bad while expression")]
    BadWhileExpression,
    #[error("Bad function call")]
    BadFunctionCall,
    #[error("Invalid control flow statement for a function")]
    InvalidControlFlowStmt,
    #[error("Wrong number of arguments to a function")]
    WrongNumArgsFun,
    #[error("Constructor must be a function")]
    ConstructorMustBeFunc,
    #[error("Bad access of a field or method")]
    BadGetExpression,
    #[error("Bad access of a static field or method")]
    BadStaticGetExpression,
    #[error("Bad this expression")]
    BadThisExpression,
    #[error("Bad set expression")]
    BadSetExpression,
    #[error("You're trying to access something that isn't an array")]
    BadArraySetExpression,
    #[error("Bad statements inside a struct")]
    BadStmtInsideStruct,
    #[error("Invalid array access")]
    InvalidArrayAccess,
    #[error("The overloaded operator isn't of the right type")]
    OverloadNotOfRightType,
    #[error("Failed to open file being included")]
    FailedToInclude,
}

#[derive(Clone, Debug)]
pub enum ControlFlowType {
    Break,
    Continue,
    Return(Expression),
    None,
}

pub trait Accept<Visitor> {
    type Output;
    fn accept(&self, visitor: &mut Visitor) -> Self::Output;
}


pub trait ExprVisitor {
    type Output;

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Output;
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::Output;
    fn visit_array_expr(&mut self, expr: &ArrayExpr) -> Self::Output;

    fn visit_cast_expr(&mut self, expr: &CastExpr) -> Self::Output;
    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> Self::Output;

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::Output;
    fn visit_variable_expr(&mut self, expr: &VarExpr) -> Self::Output;
    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::Output;

    fn visit_array_get_expr(&mut self, expr: &ArrayGetExpr) -> Self::Output;
    fn visit_get_expr(&mut self, expr: &GetExpr) -> Self::Output;

    fn visit_this_expr(&mut self, expr: &ThisExpr) -> Self::Output;
}

#[derive(Clone, Debug, EnumAsInner)]
pub enum Expression {
    Literal(Box<LiteralExpr>),
    Binary(Box<BinaryExpr>),
    Array(Box<ArrayExpr>),
    Cast(Box<CastExpr>),
    Grouping(Box<GroupingExpr>),
    Unary(Box<UnaryExpr>),
    Variable(Box<VarExpr>),
    Call(Box<CallExpr>),
    ArrayGet(Box<ArrayGetExpr>),
    Get(Box<GetExpr>),
    This(Box<ThisExpr>),
}

impl From<LiteralExpr> for Expression {
    fn from(value: LiteralExpr) -> Self {
        Expression::Literal(Box::new(value))
    }
}

impl From<BinaryExpr> for Expression {
    fn from(value: BinaryExpr) -> Self {
        Expression::Binary(Box::new(value))
    }
}

impl From<ArrayExpr> for Expression {
    fn from(value: ArrayExpr) -> Self {
        Expression::Array(Box::new(value))
    }
}

impl From<CastExpr> for Expression {
    fn from(value: CastExpr) -> Self {
        Expression::Cast(Box::new(value))
    }
}

impl From<GroupingExpr> for Expression {
    fn from(value: GroupingExpr) -> Self {
        Expression::Grouping(Box::new(value))
    }
}

impl From<UnaryExpr> for Expression {
    fn from(value: UnaryExpr) -> Self {
        Expression::Unary(Box::new(value))
    }
}

impl From<VarExpr> for Expression {
    fn from(value: VarExpr) -> Self {
        Expression::Variable(Box::new(value))
    }
}

impl From<ArrayGetExpr> for Expression {
    fn from(value: ArrayGetExpr) -> Self {
        Expression::ArrayGet(Box::new(value))
    }
}

impl From<ThisExpr> for Expression {
    fn from(value: ThisExpr) -> Self {
        Expression::This(Box::new(value))
    }
}

pub trait StmtVisitor {
    type Output;

    fn visit_expression_stmt(&mut self, expr: &ExprStmt) -> Self::Output;

    fn visit_variable_stmt(&mut self, expr: &VarStmt) -> Self::Output;
    fn visit_function_stmt(&mut self, expr: &FnStmt) -> Self::Output;
    fn visit_struct_stmt(&mut self, expr: &StructStmt) -> Self::Output;

    fn visit_block_stmt(&mut self, expr: &BlockStmt) -> Self::Output;

    fn visit_if_stmt(&mut self, expr: &IfStmt) -> Self::Output;
    fn visit_while_stmt(&mut self, expr: &WhileStmt) -> Self::Output;
    fn visit_for_stmt(&mut self, expr: &ForStmt) -> Self::Output;

    fn visit_ctrl_stmt(&mut self, expr: &CtrlStmt) -> Self::Output;
    fn visit_include_stmt(&mut self, expr: &IncludeStmt) -> Self::Output;

    fn visit_set_stmt(&mut self, expr: &SetStmt) -> Self::Output;
    fn visit_array_set_stmt(&mut self, expr: &ArraySetStmt) -> Self::Output;
}

#[derive(Clone, Debug, EnumAsInner)]
pub enum Statement {
    Expression(Box<ExprStmt>),
    Block(Box<BlockStmt>),
    If(Box<IfStmt>),
    While(Box<WhileStmt>),
    For(Box<ForStmt>),
    Variable(Box<VarStmt>),
    Function(Box<FnStmt>),
    Struct(Box<StructStmt>),
    ControlFlow(Box<CtrlStmt>),
    Include(Box<IncludeStmt>),
    Set(Box<SetStmt>),
    ArraySet(Box<ArraySetStmt>),
    Static(Box<StaticStmt>),
}

#[derive(Clone, Debug)]
pub struct ExprStmt {
    pub expression: Expression,
}

impl ExprStmt {
    pub fn new(expression: Expression) -> Self {
        Self { expression }
    }
}

impl From<Expression> for ExprStmt {
    fn from(value: Expression) -> Self {
        Self { expression: value }
    }
}

impl From<ExprStmt> for Statement {
    fn from(value: ExprStmt) -> Self {
        Self::Expression(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub struct BlockStmt {
    pub statements: Vec<Statement>,
    pub is_standalone: bool,
}

impl BlockStmt {
    pub fn new(statements: Vec<Statement>, is_standalone: bool) -> Self {
        Self {
            statements,
            is_standalone,
        }
    }
}

impl From<(Vec<Statement>, bool)> for BlockStmt {
    fn from(value: (Vec<Statement>, bool)) -> Self {
        Self {
            statements: value.0,
            is_standalone: value.1,
        }
    }
}

impl From<BlockStmt> for Statement {
    fn from(value: BlockStmt) -> Self {
        Statement::Block(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub enum ElseStmt {
    Else(Box<BlockStmt>),
    ElseIf(Box<IfStmt>),
}

impl From<(Vec<Statement>, bool)> for ElseStmt {
    //from raw block
    fn from(value: (Vec<Statement>, bool)) -> Self {
        Self::Else(Box::new(value.into()))
    }
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Expression,
    pub then: BlockStmt,
    pub or: Option<ElseStmt>,
}

impl IfStmt {
    pub fn new(cond: Expression, then: Vec<Statement>, or: Option<ElseStmt>) -> Self {
        Self {
            cond,
            then: BlockStmt::new(then, false),
            or,
        }
    }
}

impl From<IfStmt> for Statement {
    fn from(value: IfStmt) -> Self {
        Statement::If(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub struct WhileStmt {
    pub cond: Expression,
    pub then: BlockStmt,
    pub incr_stmt: Option<Statement>, // this only exists in case we're doing a for loop
}

impl WhileStmt {
    pub fn new(cond: Expression, then: Vec<Statement>, incr_stmt: Option<Statement>) -> Self {
        Self {
            cond,
            then: BlockStmt::new(then, false),
            incr_stmt,
        }
    }
}

impl From<WhileStmt> for Statement {
    fn from(value: WhileStmt) -> Self {
        Statement::While(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub struct ForStmt {
    pub decl: Option<Statement>,
    pub iwhile: WhileStmt,
}

impl ForStmt {
    pub fn new(
        decl: Option<Statement>,
        cond: Option<Expression>,
        incr: Option<Statement>,
        then: Vec<Statement>,
    ) -> Self {
        Self {
            decl,
            iwhile: WhileStmt::new(
                match cond {
                    Some(rcond) => rcond,
                    None => LiteralExpr::new(Immediate::Boolean(true)).into(),
                },
                then,
                incr,
            ),
        }
    }
}

impl From<ForStmt> for Statement {
    fn from(value: ForStmt) -> Self {
        Statement::For(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub struct CtrlStmt {
    pub ctrl: ControlFlowType,
}

impl CtrlStmt {
    pub fn new(ctrl: ControlFlowType) -> Self {
        Self { ctrl }
    }
}

impl From<CtrlStmt> for Statement {
    fn from(value: CtrlStmt) -> Self {
        Statement::ControlFlow(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub struct IncludeStmt {
    pub file: String
}

impl IncludeStmt{
    pub fn new(file: String) -> Self {
        Self { file }
    }
}

impl From<IncludeStmt> for Statement {
    fn from(value: IncludeStmt) -> Self {
        Statement::Include(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub struct VarStmt {
    pub name: Token,
    pub init: Expression,
}

impl VarStmt {
    pub fn new(name: Token, init: Expression) -> Self {
        Self { name, init }
    }
}

impl From<VarStmt> for Statement {
    fn from(value: VarStmt) -> Self {
        Self::Variable(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub struct FnStmt {
    pub name: Token,
    pub params: Vec<String>,
    pub body: Vec<Statement>,
}

impl FnStmt {
    pub fn new(name: Token, params: Vec<String>, body: Vec<Statement>) -> Self {
        Self { name, params, body }
    }
}

impl From<FnStmt> for Statement {
    fn from(value: FnStmt) -> Self {
        Self::Function(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub struct StructStmt {
    pub name: Token,
    pub methods: Vec<Statement>, // a subset of statements, corresponding only to declarations
}

impl StructStmt {
    pub fn new(name: Token, methods: Vec<Statement>) -> Self {
        Self { name, methods }
    }
}

impl From<StructStmt> for Statement {
    fn from(value: StructStmt) -> Self {
        Self::Struct(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub struct LiteralExpr {
    pub value: Immediate,
}

impl LiteralExpr {
    pub fn new(value: Immediate) -> Self {
        Self { value }
    }
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub left: Expression,
    pub op: Token,
    pub right: Expression,
}

impl BinaryExpr {
    pub fn new(left: Expression, op: Token, right: Expression) -> Self {
        Self { left, op, right }
    }
}

#[derive(Clone, Debug)]
pub struct ArrayExpr {
    pub exprs: Vec<Expression>,
}

impl ArrayExpr {
    pub fn new(exprs: Vec<Expression>) -> Self {
        Self { exprs }
    }
}

#[derive(Clone, Debug)]
pub struct CastExpr {
    pub op: Token,
    pub argument: Expression,
}

impl CastExpr {
    pub fn new(op: Token, argument: Expression) -> Self {
        Self { op, argument }
    }
}

#[derive(Clone, Debug)]
pub struct GroupingExpr {
    // ()
    pub expr: Expression,
}

impl GroupingExpr {
    pub fn new(expr: Expression) -> Self {
        Self { expr }
    }
}

#[derive(Clone, Debug)]
pub struct ThisExpr {
    pub token: Token,
}

impl ThisExpr {
    pub fn new(token: Token) -> Self {
        Self { token }
    }
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub op: Token,
    pub right: Expression,
}

impl UnaryExpr {
    pub fn new(op: Token, right: Expression) -> Self {
        Self { op, right }
    }
}

#[derive(Clone, Debug)]
pub struct VarExpr {
    pub name: Token,
}

impl VarExpr {
    pub fn new(name: Token) -> Self {
        Self { name }
    }
}

#[derive(Clone, Debug)]
pub struct ArrayGetExpr {
    pub callee: Expression,
    pub argument: Expression,
}

impl ArrayGetExpr {
    pub fn new(callee: Expression, argument: Expression) -> Self {
        Self { callee, argument }
    }
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub callee: Expression,
    pub paren_tok: Option<Token>,
    pub arguments: Vec<Expression>,
}

impl CallExpr {
    pub fn new(callee: Expression, paren_tok: Option<Token>, arguments: Vec<Expression>) -> Self {
        Self {
            callee,
            paren_tok,
            arguments,
        }
    }
}

impl From<CallExpr> for Expression {
    fn from(value: CallExpr) -> Self {
        Self::Call(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub struct GetExpr {
    pub callee: Expression,
    pub argument: Token,
    pub is_static: bool,
}

impl GetExpr {
    pub fn new(callee: Expression, argument: Token, is_static: bool) -> Self {
        Self {
            callee,
            argument,
            is_static,
        }
    }
}

impl From<GetExpr> for Expression {
    fn from(value: GetExpr) -> Self {
        Self::Get(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub struct SetStmt {
    pub callee: Expression, // this is basically a GetExpr
    pub name: Token,
    pub op: Token,
    pub rvalue: Expression,
}

impl SetStmt {
    pub fn new(callee: Expression, op: Token, name: Token, rvalue: Expression) -> Self {
        Self {
            callee,
            op,
            name,
            rvalue,
        }
    }
}

impl From<SetStmt> for Statement {
    fn from(value: SetStmt) -> Self {
        Self::Set(Box::new(value))
    }
}

#[derive(Clone, Debug)]
pub struct ArraySetStmt {
    pub callee: Expression, // this is basically a GetExpr
    pub argument: Expression,
    pub op: Token,
    pub rvalue: Expression,
}

impl ArraySetStmt {
    pub fn new(callee: Expression, op: Token, argument: Expression, rvalue: Expression) -> Self {
        Self {
            callee,
            op,
            argument,
            rvalue,
        }
    }
}

impl From<ArraySetStmt> for Statement {
    fn from(value: ArraySetStmt) -> Self {
        Self::ArraySet(Box::new(value))
    }
}
#[derive(Clone, Debug)]
pub struct StaticStmt {
    pub inner: Statement,
}

impl StaticStmt {
    pub fn new(inner: Statement) -> Self {
        Self { inner }
    }
}

impl From<StaticStmt> for Statement {
    fn from(value: StaticStmt) -> Self {
        Self::Static(Box::new(value))
    }
}

impl<Visitor, T> Accept<Visitor> for LiteralExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_literal_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for BinaryExpr 
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_binary_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for UnaryExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_unary_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for CastExpr 
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_cast_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for GroupingExpr 
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_grouping_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for ExprStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_expression_stmt(self)
    }
}


impl<Visitor, T> Accept<Visitor> for VarExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_variable_expr(self)
    }
}

impl<Visitor, T> Accept<Visitor> for CallExpr
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_call_expr(self)

    }
}

impl<Visitor, T> Accept<Visitor> for VarStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_variable_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for BlockStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_block_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for IfStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_if_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for WhileStmt
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_while_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for ForStmt 
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        visitor.visit_for_stmt(self)
    }
}

impl<Visitor, T> Accept<Visitor> for Expression
where
    Visitor: ExprVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        match *self {
            Expression::Literal(ref literal) => literal.accept(visitor),
            Expression::Binary(ref binary) => binary.accept(visitor),
            Expression::Unary(ref unary) => unary.accept(visitor),
            Expression::Grouping(ref group) => group.accept(visitor),
            Expression::Variable(ref var) => var.accept(visitor),
            Expression::Call(ref call) => call.accept(visitor),
            Expression::Cast(ref cast) => cast.accept(visitor),
            _ => unimplemented!()
        }
    }
}

impl<Visitor, T> Accept<Visitor> for Statement
where
    Visitor: StmtVisitor<Output = T>,
{
    type Output = T;

    fn accept(&self, visitor: &mut Visitor) -> Self::Output {
        match *self {
            Statement::Expression(ref expr) => expr.accept(visitor),
            Statement::Variable(ref var) => var.accept(visitor),
            Statement::Block(ref block) => block.accept(visitor),
            Statement::If(ref ifstmt) => ifstmt.accept(visitor),
            Statement::While(ref whil) => whil.accept(visitor),
            Statement::For(ref _for) => _for.accept(visitor),
            _ => unimplemented!()
        }
    }
}
