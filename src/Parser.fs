module Parser
#nowarn "25"

open Representation
open Util
open State

//Parser exceptions
exception ParserException of string * (int * int)

//State of parser at any point
type ParserState = {
  Line: int
  Column: int
  Tokens: Token list
}

//Expressions
type Expr =
  | BoolExpr of bool
  | StringExpr of string
  | NumberExpr of double 
  | UnaryExpr of TokenType * Expr
  | BinaryExpr of Expr * TokenType * Expr
  | VarDeclExpr of string * Expr
  | VarGetExpr of string
  | CallExpr of string * Expr list

type Precedence =
  | None        = 0
  | Assignment  = 1
  | Or          = 2
  | And         = 3
  | Equality    = 4
  | Comparison  = 5
  | Sum         = 6
  | Product     = 7
  | Unary       = 8
  | Call        = 9
  | Primary     = 10

//Monadic type for parser state, parser combinators
type ParserM<'T> = StateM<'T, ParserState>
type ParserCombinator = ParserM<Expr>


let fail : ParserM<unit> = state {
  let! parser = get
  raise <| ParserException ("Unexpected token", (parser.Line, parser.Column))
}

let unpackOrFail operand : ParserM<'a> = state {
  let! parser = get
  if Option.isNone operand then
    do! fail
  return Option.get operand
}

//Check if end of source is reached
let isAtEnd : ParserM<bool> = state {
  let! parser = get
  return parser.Tokens.Length = 0
}

//Advance 1 character, updating the parsers state, and return this character
let advance : ParserM<Token> = state {
  let! parser = get
  let res = parser.Tokens.[0]
  do! set { parser with 
              Tokens = parser.Tokens.[1..]
              Line = res.Line
              Column = res.Column }
  return res
}

//Advance 1 character, ignore it
let advanceIgnore : ParserM<unit> = state {
  let! _ = advance
  ()
}

//Peek the next character
let peek : ParserM<Token> = state {
  let! parser = get
  return parser.Tokens.[0]
}

let eat expected : ParserM<unit> = state {
  let! next = peek
  if next.Type = expected then
    do! advanceIgnore
  else
    do! fail
}

//Precedence levels
let getInfixPrecedence token =
  match token.Type with
  | Equal                                     -> Precedence.Assignment
  | Or                                        -> Precedence.Or
  | And                                       -> Precedence.And
  | EqualEqual | BangEqual                    -> Precedence.Equality
  | Less | LessEqual | Greater | GreaterEqual -> Precedence.Comparison
  | Plus | Minus                              -> Precedence.Sum
  | Asterisk | Slash | Modulo                 -> Precedence.Product
  | _ -> Precedence.None

let parsePrefixOperator parseExpr token : ParserCombinator = state {
  let! expr = parseExpr Precedence.Unary
  return UnaryExpr (token.Type, expr)
}

let parseNumber parseExpr token : ParserCombinator = state {
  let (Number n) = token.Type
  return NumberExpr n
}

let parseGroup parseExpr token : ParserCombinator = state {
  let! expr = parseExpr Precedence.None
  do! eat RightParen
  return expr
}

let getPrefixParser token =
  match token.Type with
  | Plus | Minus | Bang -> Some parsePrefixOperator
  //| Identifier _ -> Some parseName
  | Number _ -> Some parseNumber
  | LeftParen -> Some parseGroup
  | _ -> None

let parseInfixOperator parseExpr left token : ParserCombinator = state {
  let! right = parseExpr (getInfixPrecedence token)
  return BinaryExpr (left, token.Type, right)
}

let getOtherfixParser token =
  match token.Type with
  | Plus | Minus | Asterisk | Slash -> Some parseInfixOperator
  | _ -> None

let rec parseExpr precedence = state {
  let! parser = get
  let! first = advance
  let! prefix = getPrefixParser first |> unpackOrFail
  let! left = prefix parseExpr first
  let rec loop (left: Expr) = state {
    let! atEnd = isAtEnd
    if not atEnd then
      let! next = peek
      if precedence < getInfixPrecedence next then
        do! advanceIgnore
        let! otherfix = getOtherfixParser next |> unpackOrFail
        let! left = otherfix parseExpr left next
        return! loop left
      else return left
    else return left
  }
  return! loop left
}

let parser = {
  Line = 1
  Column = 1
  Tokens = Lexer.lex "-(5+2)*7"
}

parseExpr Precedence.None parser |> printfn "%A" 
