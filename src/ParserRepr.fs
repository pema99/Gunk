module ParserRepr

open LexerRepr
open Combinator

type ParserState =
  { Tokens: Token list
    Line: int
    Column: int }
  interface CombinatorState<TokenType> with
    member this.Peek = com {
      if this.Tokens.Length > 0 then
        return this.Tokens.[0].Type
      else
        return! fail()
    }
    member this.Item = com {
      if this.Tokens.Length > 0 then
        let res = this.Tokens.[0]
        let updated = { this with
                          Tokens = this.Tokens.[1..]
                          Line = res.Line
                          Column = res.Column }
        do! com.set (updated :> CombinatorState<TokenType>)
        return res.Type
      else
        return! fail()
    }

type Parser<'T> = Com<'T, TokenType>

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

let tokenToExprType token : Parser<ExprType> =
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

