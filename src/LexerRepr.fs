module LexerRepr

open Combinator

type LexerState =
  { Source: string
    Line: int
    Column: int }
  interface CombinatorState<char> with
    member this.Peek = com {
      if this.Source.Length > 0 then
        return this.Source.[0]
      else
        return! fail()
    }
    member this.Item = com {
      if this.Source.Length > 0 then
        let res = this.Source.[0]
        let updated = { this with
                          Source = this.Source.[1..]
                          Line = if res = '\n' then this.Line + 1 else this.Line
                          Column = if res = '\n' then 1 else this.Column + 1}
        do! com.set (updated :> CombinatorState<char>)
        return res
      else
        return! fail()
    }

type TokenType = 
  // Algebraic operators
  | Plus
  | Minus
  | Slash
  | Asterisk
  | Power
  | Modulo
  // Comparison operators
  | EqualEqual
  | Bang
  | BangEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | And
  | Or
  | Xor
  // Scoping operators
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  // Misc operators
  | Equal
  | Comma
  | Dot
  | Arrow
  // Reserved keywords
  | If
  | Else
  | Func
  | Return
  | True
  | False
  // Primitive type keywords
  | Struct
  | Str
  | F32
  | F64
  | I8
  | I16
  | I32
  | I64
  | U8
  | U16
  | U32
  | U64
  | Bool
  | Unit
  // Literals and identifiers
  | String of string 
  | Number of float
  | Identifier of string
  //Intrinsics
  | Print
  | PrintLine
  | Clear

type Token = {
  Type: TokenType
  Line: int
  Column: int
}

let operatorMap = Map [
  "+",  Plus
  "-",  Minus 
  "/",  Slash
  "*",  Asterisk
  "^",  Power
  "%",  Modulo
  "(",  LeftParen
  ")",  RightParen
  "{",  LeftBrace
  "}",  RightBrace
  "=",  Equal
  ",",  Comma
  ".",  Dot
  "!",  Bang
  ">",  Greater
  "<",  Less
  "==", EqualEqual
  "!=", BangEqual
  ">=", GreaterEqual
  "<=", LessEqual
  "&&", And
  "||", Or
  "^^", Xor
  "->", Arrow
]

let keywordMap = Map [
  "if",      If
  "else",    Else
  "func",    Func
  "return",  Return
  "true",    True
  "false",   False
  "struct",  Struct
  "string",  Str
  "f32",     F32
  "f64",     F64
  "i8",      I8
  "i16",     I16
  "i32",     I32
  "i64",     I64
  "u8",      U8
  "u16",     U16
  "u32",     U32
  "u64",     U64
  "bool",    Bool
  "unit",    Unit
  "print",   Print
  "println", PrintLine
  "cls",     Clear
]

//Active pattern for matching tokens with single length
let (|Operator|_|) c =
  if operatorMap.ContainsKey c then
    Some operatorMap.[c]
  else
    None

//Active pattern for matching the first char of an operator
let (|OperatorStart|InvalidOperator|) c =
  if operatorMap |> Map.exists (fun k _ -> k.[0] = c) then
    OperatorStart
  else
    InvalidOperator

//Active pattern for matching keywords
let (|Keyword|_|) k =
  if keywordMap.ContainsKey k then
    Some keywordMap.[k]
  else
    None
