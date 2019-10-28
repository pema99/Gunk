module Representation

type TokenType = 
  // Algebraic operators
  | Plus
  | Minus
  | Slash
  | Asterisk
  | Power
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
  // Literals and identifiers
  | String of string 
  | Number of float
  | Identifier of string

type Token = {
  Type: TokenType
  Line: int
  Column: int
}

let operatorMap = Map [
  "+", Plus
  "-", Minus 
  "/", Slash
  "*", Asterisk
  "^", Power
  "(", LeftParen
  ")", RightParen
  "{", LeftBrace
  "}", RightBrace
  "=", Equal
  ",", Comma
  ".", Dot
  "!", Bang
  ">", Greater
  "<", Less
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
  "if",     If
  "else",   Else
  "func",   Func
  "return", Return
  "true",   True
  "false",  False
  "struct", Struct
  "string", Str
  "f32",    F32
  "f64",    F64
  "i8",     I8
  "i16",    I16
  "i32",    I32
  "i64",    I64
  "u8",     U8
  "u16",    U16
  "u32",    U32
  "u64",    U64
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
