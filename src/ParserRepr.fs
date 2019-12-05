module ParserRepr

open LexerRepr
open State

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

let combineFailure a b =
  match a, b with
  | Success _, Success _                 -> Failure
  | Success _, Failure
  | Success _, FailureWith _
  | Success _, CompoundFailure _         -> b 
  | Failure, Success _         
  | Failure, Failure                     -> Failure
  | Failure, FailureWith _
  | Failure, CompoundFailure _           -> b
  | FailureWith _, Success _
  | FailureWith _, Failure               -> a
  | FailureWith l, FailureWith r         -> CompoundFailure [l; r] 
  | FailureWith l, CompoundFailure r     -> CompoundFailure (l :: r)
  | CompoundFailure _, Success _
  | CompoundFailure _, Failure           -> a
  | CompoundFailure l, FailureWith r     -> CompoundFailure (r :: l)
  | CompoundFailure l, CompoundFailure r -> CompoundFailure (l @ r)

let copyFailure a =
  match a with
  | Success _         -> Failure
  | Failure           -> Failure
  | FailureWith e     -> FailureWith e
  | CompoundFailure e -> CompoundFailure e
  
type Com<'T> = StateM<ParserResult<'T>, ParserState>

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

let tokenToExprType token : Com<ExprType> =
  fun s ->
    (match token with
    | TokenType.Unit            -> Success Unit
    | TokenType.U8              -> Success U8
    | TokenType.U16             -> Success U16
    | TokenType.U32             -> Success U32
    | TokenType.U64             -> Success U64
    | TokenType.I8              -> Success I8
    | TokenType.I16             -> Success I16
    | TokenType.I32             -> Success I32
    | TokenType.I64             -> Success I64
    | TokenType.F32             -> Success F32
    | TokenType.F64             -> Success F64
    | TokenType.Str             -> Success Str
    | TokenType.Bool            -> Success Bool
    | TokenType.Identifier name -> Success (Struct name)
    | _ -> Failure), s

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

