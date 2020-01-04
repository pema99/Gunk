module Lexer

#nowarn "40"

open Util
open LexerRepr
open Combinator
open System

type Lexer<'T> = Com<'T, char>

let eatWhile1 pred : Lexer<string> =
  many1 (satisfy pred) |>> String.Concat

let eatWhile pred : Lexer<string> =
  many (satisfy pred) |>> String.Concat

let numberP : Lexer<TokenType> = 
  eatWhile1 (fun c -> isNumeric c || c = '.')
  |>> float
  |>> Number

let stringP : Lexer<TokenType> =
  let bodyP = 
    eatWhile1 ((<>) '"') 
    |>> TokenType.String
  between (one '"') bodyP (one '"')

let identifierP : Lexer<TokenType> =
  (eatWhile1 isAlpha <+> eatWhile isAlphanumeric)
  |>> uncurry (+)
  |>> fun s ->
    match s with
    | Keyword k -> k
    | _ -> Identifier s

let operatorP : Lexer<TokenType> = 
  let rec loop acc : Lexer<TokenType> = com {
    match acc with
    | Operator o -> return o
    | _ -> 
      let! next = item
      return! loop (acc + string next)
  }
  satisfy (fun c -> 
    match c with 
    | OperatorStart -> true 
    | InvalidOperator -> false)
  |>> string
  >>= loop

let whitespaceP : Lexer<unit> =
  many (satisfy isWhitespace) 
  |>> ignore

let tokenTypeP : Lexer<TokenType> =
  let validP =
    operatorP
    <|> identifierP
    <|> numberP
    <|> stringP
  whitespaceP *> validP

let testState = {
  Source = "i32 fib (i32 n) {
  print 2+3
}"
  Line = 1
  Column = 1
}

let lexP : Lexer<Token list> = 
  let tokenP : Lexer<Token> = com {
    let! state = com.get()
    let state = (state :?> LexerState)
    let! tokenType = tokenTypeP
    let res = {
      Type = tokenType
      Line = state.Line
      Column = state.Column
    }
    return res
  }
  many tokenP

let lex source = 
  let init = {
    Source = source
    Line = 1
    Column = 1
  }
  lexP init |> fst
