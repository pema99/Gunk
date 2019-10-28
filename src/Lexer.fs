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

//Check if end of source is reached
let isAtEnd : LexerM<bool> = state {
  let! lexer = get
  return lexer.Source.Length = 0
}

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

//Advance 1 character, ignore it
let advanceIgnore : LexerM<unit> = state {
  let! _ = advance
  ()
}

//Peek the next character
let peek : LexerM<char> = state {
  let! lexer = get
  return lexer.Source.[0]
}

//TODO: Make this tail-recursive
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
  let rec matchOperator maxLen = state {
    let! lexer = get
    if lexer.Source.Length < maxLen then
      return! matchOperator (maxLen-1)
    else
      match lexer.Source.[..maxLen-1] with
      | Operator op ->
        for i=0 to maxLen do
          do! advanceIgnore
        return! makeToken op
      | _ -> return! matchOperator (maxLen-1)
  }
  return! matchOperator 3
}

//Advance one language atom, skipping whitespace, and returning None if end is reached
let rec eatAtom : LexerM<Token option> = state { 
  let! atEnd = isAtEnd
  if atEnd then return None
  else
    match! peek with
    | OperatorStart -> 
      let! op = eatOperator
      return Some op
    | Alpha c ->
      let! iden = eatIdentifier
      let tokenType = 
        match iden with
        | Keyword k -> k
        | _ -> Identifier iden 
      let! res = makeToken tokenType
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
      do! advanceIgnore
      return! eatAtom
    | c -> 
      let! lexer = get
      let pos = lexer.Line, lexer.Column
      raise <| LexerException (sprintf "Invalid token '%c'" c, pos)
      return None
}

//Run the lexer over the given source, returning a list of tokens
let lex lexer =
  let rec lexCont tokens = state {
    let! atEnd = isAtEnd
    if atEnd then return tokens
    else
      let! token = eatAtom
      match token with
      | Some t -> return! lexCont (t :: tokens) 
      | None -> return tokens
  }
  lexCont [] lexer
    |> fst
    |> List.rev

let init = {
  Line = 1
  Column = 1
  Source = "
i32 fib(i32 n) {
  if n <= 1 {
    n
  }
  else {
    fib(n-1) + fib(n-2)
  }
}
"
}

try
  lex init |> printfn "%A"
with
  | LexerException (err, pos) ->
    printfn "Error%A: %s" pos err
//eatIdentifier "hello__%!" |> printfn "%A"
