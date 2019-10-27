#nowarn "40"

open Representation
open Util
open State

//Lexer exceptions
exception LexerException of string * (int * int)

//State of lexer at any point
type LexerState = {
  Line: int
  Column: int
  Source: string
}

//Monadic type for lexer state
type LexerM<'T> = StateM<'T, LexerState>

//Make token at current position, taking into account multi-character atoms
let makeToken tokenType =
  fun s ->
    let offset = 
      match tokenType with
      | String str -> str.Length + 1
      | Number num -> (string num).Length
      | Identifier iden -> iden.Length
      | _ -> 0
    { Type = tokenType
      Line = s.Line
      Column = s.Column - offset },
    s

//Advance 1 character, updating the lexers state, and return this character
let advance : LexerM<char> = state {
  let! lexer = get
  let res = lexer.Source.[0]
  let newLine = res = '\n'
  do! set { lexer with 
              Source = lexer.Source.[1..]
              Line = if newLine then lexer.Line + 1 else lexer.Line
              Column = if newLine then 1 else lexer.Column + 1 }
  return res
}

//Peek the next character
let peek : LexerM<char> = state {
  let! lexer = get
  return lexer.Source.[0]
}

//Check if end of source is reached
let isAtEnd : LexerM<bool> = state {
  let! lexer = get
  return lexer.Source.Length = 0
}

//Keep advancing and appending yielded chars to a string while predicate holds
let rec eatWhile pred : LexerM<string> = state {
  let! atEnd = isAtEnd
  if atEnd then 
    return ""
  else
    let! peeked = peek
    if pred peeked then
      let! curr = advance
      let! next = eatWhile pred
      return string curr + next
    else 
      return ""
}

//Advance one identifier
let eatIdentifier : LexerM<string> = eatWhile isAlphanumeric

//Advance one numeric literal
let eatNumber : LexerM<string> = eatWhile (fun c -> isNumeric c || c = '.')

//Advance one string literal and return it
let eatString : LexerM<string> = state {
  let! lexer = get
  let startPos = lexer.Line, lexer.Column
  let! startDelim = advance 
  let! inner = eatWhile (fun c -> c <> '"')
  let! atEnd = isAtEnd
  if atEnd then
    raise <| LexerException ("Undelimited string", startPos)
  let! endDelim = advance
  return inner
}

//Advance one operator and return it
let eatOperator : LexerM<Token> = state {
  let! curr = advance
  let tokenType = 
    match curr with
    | Operator op -> op
    | _ -> raise <| LexerException ("Invalid operator", (0, 0))
  return! makeToken tokenType
}

//Advance one language atom, skipping whitespace, and returning None if end is reached
let rec eatAtom : LexerM<Token option> = state { 
  let! atEnd = isAtEnd
  if atEnd then return None
  else
    match! peek with
    | Operator _ -> 
      let! op = eatOperator
      return Some op
    | Alpha c ->
      let! iden = eatIdentifier
      let! res = makeToken (Identifier iden)
      return Some res
    | Numeric c ->
      let! num = eatNumber
      let! res = makeToken (Number (float num))
      return Some res
    | '"' ->
      let! str = eatString
      let! res = makeToken (String str)
      return Some res
    | Whitespace c -> 
      let! _ = advance
      return! eatAtom
    | c -> 
      let! lexer = get
      let pos = lexer.Line, lexer.Column
      raise <| LexerException (sprintf "Invalid token '%c'" c, pos)
      return None
}

//Run the lexer over the given source, returning a list of tokens
let lex lexer =
  let rec lexCont = state { 
    let! atEnd = isAtEnd
    if atEnd then return []
    else
      let! token = eatAtom
      match token with
      | Some t -> 
        let! next = lexCont
        return t :: next
      | None -> return []
  }
  lexCont lexer |> fst

let init = {
  Line = 1
  Column = 1
  Source = "let add a b = a + b"
}

lex init |> printfn "%A"

//eatIdentifier "hello__%!" |> printfn "%A"