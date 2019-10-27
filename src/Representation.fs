module Representation

type TokenType = 
  | Empty
  | Plus
  | Minus
  | Slash
  | Asterisk
  | Equal
  | String of string 
  | Number of float
  | Identifier of string

type Token = {
  Type: TokenType
  Line: int
  Column: int
}

//Active pattern for matching tokens with fixed lengths
let (|Operator|_|) c =
  match c with
  | '+' -> Some Plus
  | '-' -> Some Minus
  | '/' -> Some Slash
  | '*' -> Some Asterisk
  | '=' -> Some Equal
  | _ -> None