module ParserRepr

open LexerRepr
open State

type ExprType =
  | Void         // Empty set
  | Unit         // Singleton set
  | U8
  | U16
  | U32
  | U64
  | I8
  | I16
  | I32
  | I64
  | F32
  | F64
  | Str
  | Bool
  | Struct of string

let tokenToExprType token =
  match token with
  | TokenType.Unit            -> Some Unit
  | TokenType.U8              -> Some U8
  | TokenType.U16             -> Some U16
  | TokenType.U32             -> Some U32
  | TokenType.U64             -> Some U64
  | TokenType.I8              -> Some I8
  | TokenType.I16             -> Some I16
  | TokenType.I32             -> Some I32
  | TokenType.I64             -> Some I64
  | TokenType.F32             -> Some F32
  | TokenType.F64             -> Some F64
  | TokenType.Str             -> Some Str
  | TokenType.Bool            -> Some Bool
  | TokenType.Identifier name -> Some (Struct name)
  | _ -> None

//Expressions
type Expr =
  | BoolExpr of bool
  | StringExpr of string
  | NumberExpr of double 
  | UnaryExpr of TokenType * Expr
  | BinaryExpr of Expr * TokenType * Expr
  | VarGetExpr of string
  | CallExpr of string * Expr list
  | CondExpr of Expr * Stmt list * Stmt list
  | IndexExpr of string * Expr
//| MatchExpr of (Pattern * Expr) list 

and Stmt =
  | ExprStmt of Expr
  | VarDeclStmt of string * ExprType * Expr
  | FuncDeclStmt of string * ExprType * (string * ExprType) list * Stmt list
  | BlockStmt of Stmt list
  | PrintStmt of bool * Expr
  | ClearStmt

type Precedence =
  | None        = 0
  | Declaration = 1
  | Or          = 2
  | And         = 3
  | Equality    = 4
  | Comparison  = 5
  | Sum         = 6
  | Product     = 7
  | Unary       = 8
  | Call        = 9
  | Primary     = 10
  
//Precedence levels
let getInfixPrecedence token =
  match token with
  | Equal                                     -> Precedence.Declaration
  | Or                                        -> Precedence.Or
  | And                                       -> Precedence.And
  | EqualEqual | BangEqual                    -> Precedence.Equality
  | Less | LessEqual | Greater | GreaterEqual -> Precedence.Comparison
  | Plus | Minus                              -> Precedence.Sum
  | Asterisk | Slash | Modulo                 -> Precedence.Product
  | _                                         -> Precedence.None

//Parser exceptions
type ParserError = string * (int * int)

//State of parser at any point
type ParserState = {
  Line: int
  Column: int
  Tokens: Token list
}

type ParserResult<'T> =
  | Success of 'T
  | Failure
  | FailureWith of ParserError
  | CompoundFailure of ParserError list 

type Com<'T> = StateM<ParserResult<'T>, ParserState>
