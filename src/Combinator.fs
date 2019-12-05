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
      | err -> copyFailure err, n
  member this.Combine (m1: Com<'T>, m2: Com<'U>) : Com<'U> =
    fun s ->
      let a, n = m1 s
      match a with
      | Success _ -> m2 n
      | err -> copyFailure err, n
  member this.Delay (f: unit -> Com<'T>): Com<'T> =
    this.Bind (this.Return (), f)
  member this.get =
    fun s -> Success s, s
  member this.set v =
    fun _ -> Success (), v

let com = ComBuilder()

let ( <|> ) (m1: Com<'T>) (m2: Com<'T>) : Com<'T> = state {  
  match! m1 with
  | Success v -> return Success v
  | _ -> return! m2
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

let ( <* ) (m1: Com<'T>) (m2: Com<'U>) : Com<'T> = com {
  let! a = m1
  let! b = m2
  return a
}
 
let ( *> ) (m1: Com<'T>) (m2: Com<'U>) : Com<'U> = com {
  let! a = m1
  let! b = m2
  return b
}

let ( <+> ) (m1: Com<'T>) (m2: Com<'U>) : Com<'T * 'U> = com {
  let! a = m1
  let! b = m2
  return a, b
}

let ( >>= ) (m: Com<'T>) (f: 'T -> Com<'U>) : Com<'U> =
  com.Bind (m, f)

let just (a: 'T) : Com<'T> =
  com.Return a

let fail () : Com<'T> =
  fun s -> Failure, s

let failWith msg : Com<'T> =
  fun s -> FailureWith (ParserError (msg, (s.Line, s.Column))), s

let many (v: Com<'T>) : Com<'T list> = com {
  let rec loop acc = state {
    match! v with
    | Success v -> return! loop (v :: acc)
    | _ -> return Success (List.rev acc)
  }
  let! res = loop []
  return res
}

let many1 (v: Com<'T>) : Com<'T list> = com {
  let! res = many v
  if res.Length = 0 then return! fail()
  else return res
}

let opt (p: Com<'T>) : Com<'T option> =
  (p |>> Some) <|> just None

let between (l: Com<'T>) (v: Com<'U>) (r: Com<'V>) : Com<'U> =
  l *> v <* r

let sepBy1 (p: Com<'T>) (sep: Com<'U>) : Com<'T list> =
  p <+> many (sep *> p)
  |>> List.Cons

let sepBy (p: Com<'T>) (sep: Com<'U>) : Com<'T list> =
  sepBy1 p sep <|> just []

let look : Com<TokenType> = com {
  let! parser = com.get
  if parser.Tokens.Length > 0 then
    return parser.Tokens.[0].Type
  else
    return! fail()
}

let item : Com<TokenType> = com { 
  let! parser = com.get
  if parser.Tokens.Length > 0 then
    let res = parser.Tokens.[0]
    do! com.set { parser with 
                    Tokens = parser.Tokens.[1..]
                    Line = res.Line
                    Column = res.Column }
    return res.Type
  else
    return! fail()
}

let satisfy (pred: TokenType -> bool) : Com<TokenType> = com {
  let! next = look
  if pred next then
    let! _ = item
    return next
  else return! fail()
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

let chainL (p: Com<'T>) (op: Com<'T -> 'T -> 'T>) : Com<'T> = com {
  let! f = op
  let! first = p 
  let rec loop prev = state {
    match! p with
    | Success curr -> return! loop (f prev curr)
    | Failure -> return Success prev
  }
  return! loop first
}
