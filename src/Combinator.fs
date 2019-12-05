module Combinator
#nowarn "25"
#nowarn "40"

open State
open LexerRepr
open ParserRepr

type ComBuilder() =
  member this.Return (v: 'T) : Com<'T> =
    fun s -> Success v, s
  member this.ReturnFrom (m: Com<'T>) : Com<'T> =
    m
  member this.Zero () : Com<unit> =
    this.Return ()
  member this.Bind (m: Com<'T>, f: 'T -> Com<'U>) : Com<'U> =
    fun s ->
      let a, n = m s
      match a with
      | Success v -> (f v) n
      | Failure -> Failure, n
  member this.Combine (m1: Com<'T>, m2: Com<'U>) : Com<'U> =
    fun s ->
      let a, n = m1 s
      match a with
      | Success _ ->
        let b, n = m2 n
        match b with
        | Success _ -> b, n 
        | Failure -> Failure, n
      | Failure -> Failure, n
  member this.Delay (f: unit -> Com<'T>): Com<'T> =
    this.Bind (this.Return (), f)

let com = ComBuilder()

let ( <|> ) (m1: Com<'T>) (m2: Com<'T>) : Com<'T> = state {  
  match! m1 with
  | Success v -> return Success v
  | Failure -> return! m2
}

let ( <*> ) (f: Com<'T -> 'U>) (m: Com<'T>) : Com<'U> = com {
  let! a = f
  let! b = m
  return a b
}

let ( <&> ) (f: 'T -> 'U) (m: Com<'T>) : Com<'U> = com {
  let! v = m
  return f v
}

let ( |>> ) (m: Com<'T>) (f: 'T -> 'U) : Com<'U> =
  f <&> m

let ( <* ) (m1: Com<'T>) (m2: Com<'U>) : Com<'T> = state {
  let! a = m1
  match a with
  | Success _ ->
    let! b = m2
    match b with
    | Success _ -> return a
    | _ -> return Failure
  | _ -> return Failure
}
  
let ( *> ) (m1: Com<'T>) (m2: Com<'U>) : Com<'U> = state {
  let! a = m1
  match a with
  | Success _ ->
    let! b = m2
    match b with
    | Success _ -> return b
    | _ -> return Failure
  | _ -> return Failure
}

let ( <+> ) (m1: Com<'T>) (m2: Com<'U>) : Com<'T * 'U> = state {
  let! a = m1
  match a with
  | Success v1 ->
    let! b = m2
    match b with
    | Success v2 -> return Success (v1, v2)
    | _ -> return Failure
  | _ -> return Failure
}

let ( >>= ) (m1: Com<'T>) (f: 'T -> Com<'U>) : Com<'U> = state {
  match! m1 with
  | Success v -> return! f v
  | Failure -> return Failure
}

let just (a: 'T) : Com<'T> =
  fun s ->
    Success a, s

let many (v: Com<'T>) : Com<'T list> = state {
  let rec loop acc = state {
    match! v with
    | Success v -> return! loop (v :: acc)
    | Failure -> return List.rev acc
  }
  let! res = loop []
  return Success res
}

let some (v: Com<'T>) : Com<'T list> = state {
  let! (Success res) = many v
  if res.Length = 0 then return Failure
  else return Success res
}

let look : Com<TokenType> = state {
  let! parser = get
  if parser.Tokens.Length > 0 then
    let res = parser.Tokens.[0]
    return Success res.Type
  else
    return Failure
}

let item : Com<TokenType> = state { 
  let! parser = get
  if parser.Tokens.Length > 0 then
    let res = parser.Tokens.[0]
    do! set { parser with 
                Tokens = parser.Tokens.[1..]
                Line = res.Line
                Column = res.Column }
    return Success res.Type
  else
    return Failure
}

let satisfy (pred: TokenType -> bool) : Com<TokenType> = state {
  match! look with
  | Success v ->
    if pred v then
      let! _ = item
      return Success v
    else return Failure
  | Failure -> return Failure
}

let check (pred: TokenType -> bool) : Com<bool> = state {
  match! look with
  | Success v ->
    if pred v then
      return Success true
    else return Success false
  | Failure -> return Success false
}

let one (tar: TokenType) : Com<TokenType> =
  satisfy ((=) tar)

let oneOf (lst: TokenType list) : Com<TokenType> =
  satisfy (fun x -> List.contains x lst)

let numberP : Com<TokenType> = 
  satisfy (fun x -> match x with Number _ -> true | _ -> false)

let stringP : Com<TokenType> =
  satisfy (fun x -> match x with String _ -> true | _ -> false)

let identifierP : Com<TokenType> = 
  satisfy (fun x -> match x with Identifier _ -> true | _ -> false)

let chainL (p: Com<'T>) (op: Com<'T -> 'T -> 'T>) : Com<'T> = state {
  match! op with
  | Success f ->
    match! p with
    | Success first ->
      let rec loop prev = state {
        match! p with
        | Success curr -> return! loop (f prev curr)
        | Failure -> return Success prev
      }
      return! loop first
    | Failure -> return Failure
  | Failure -> return Failure
}
