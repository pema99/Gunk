module Combinator
#nowarn "25"
#nowarn "40"

open State

type ParserError = string * (int * int)

type ParserResult<'T> =
  | Success of 'T
  | Failure
  | FailureWith of ParserError
  | CompoundFailure of ParserError list 

let combineFailure a b =
  match a, b with
  | Success _, Success _                 -> Failure
  | Success _, Failure
  | Success _, FailureWith _
  | Success _, CompoundFailure _         -> b 
  | Failure, Success _         
  | Failure, Failure                     -> Failure
  | Failure, FailureWith _
  | Failure, CompoundFailure _           -> b
  | FailureWith _, Success _
  | FailureWith _, Failure               -> a
  | FailureWith l, FailureWith r         -> CompoundFailure [l; r] 
  | FailureWith l, CompoundFailure r     -> CompoundFailure (l :: r)
  | CompoundFailure _, Success _
  | CompoundFailure _, Failure           -> a
  | CompoundFailure l, FailureWith r     -> CompoundFailure (r :: l)
  | CompoundFailure l, CompoundFailure r -> CompoundFailure (l @ r)

let copyFailure a =
  match a with
  | Success _         -> Failure
  | Failure           -> Failure
  | FailureWith e     -> FailureWith e
  | CompoundFailure e -> CompoundFailure e

type CombinatorState<'T> =
  abstract member Peek : Com<'T, 'T>
  abstract member Item : Com<'T, 'T>

and Com<'T, 'S> = StateM<ParserResult<'T>, CombinatorState<'S>>

type ComBuilder() =
  member this.Return (v: 'T) : Com<'T, 'S> =
    fun s -> Success v, s
  member this.ReturnFrom (m: Com<'T, 'S>) : Com<'T, 'S> =
    m
  member this.Zero () : Com<unit, 'S> =
    this.Return ()
  member this.Bind (m: Com<'T, 'S>, f: 'T -> Com<'U, 'S>) : Com<'U, 'S> =
    fun s ->
      let a, n = m s
      match a with
      | Success v -> (f v) n
      | err -> copyFailure err, n
  member this.Combine (m1: Com<'T, 'S>, m2: Com<'U, 'S>) : Com<'U, 'S> =
    fun s ->
      let a, n = m1 s
      match a with
      | Success _ -> m2 n
      | err -> copyFailure err, n
  member this.Delay (f: unit -> Com<'T, 'S>): Com<'T, 'S> =
    this.Bind (this.Return (), f)
  member this.get() =
    fun s -> Success s, s
  member this.set v =
    fun _ -> Success (), v

let com = ComBuilder()

let ( <|> ) (m1: Com<'T, 'S>) (m2: Com<'T, 'S>) : Com<'T, 'S> = state {  
  match! m1 with
  | Success v -> return Success v
  | _ -> return! m2
}

let look : Com<'T, 'T> =
  fun s -> s.Peek s

let item : Com<'T, 'T> =
  fun s -> s.Item s

let ( <*> ) (f: Com<'T -> 'U, 'S>) (m: Com<'T, 'S>) : Com<'U, 'S> = com {
  let! a = f
  let! b = m
  return a b
}

let ( <&> ) (f: 'T -> 'U) (m: Com<'T, 'S>) : Com<'U, 'S> = com {
  let! v = m
  return f v
}

let ( |>> ) (m: Com<'T, 'S>) (f: 'T -> 'U) : Com<'U, 'S> =
  f <&> m

let ( <* ) (m1: Com<'T, 'S>) (m2: Com<'U, 'S>) : Com<'T, 'S> = com {
  let! a = m1
  let! b = m2
  return a
}
 
let ( *> ) (m1: Com<'T, 'S>) (m2: Com<'U, 'S>) : Com<'U, 'S> = com {
  let! a = m1
  let! b = m2
  return b
}

let ( <+> ) (m1: Com<'T, 'S>) (m2: Com<'U, 'S>) : Com<'T * 'U, 'S> = com {
  let! a = m1
  let! b = m2
  return a, b
}

let ( >>= ) (m: Com<'T, 'S>) (f: 'T -> Com<'U, 'S>) : Com<'U, 'S> =
  com.Bind (m, f)

let just (a: 'T) : Com<'T, 'S> =
  com.Return a

let fail () : Com<'T, 'S> =
  fun s -> Failure, s

let failWith msg : Com<'T, 'S> =
  fun s -> FailureWith msg, s

let many (v: Com<'T, 'S>) : Com<'T list, 'S> = com {
  let rec loop acc = state {
    match! v with
    | Success v -> return! loop (v :: acc)
    | _ -> return Success (List.rev acc)
  }
  let! res = loop []
  return res
}

let many1 (v: Com<'T, 'S>) : Com<'T list, 'S> = com {
  let! res = many v
  if res.Length = 0 then return! fail()
  else return res
}

let opt (p: Com<'T, 'S>) : Com<'T option, 'S> =
  (p |>> Some) <|> just None

let between (l: Com<'T, 'S>) (v: Com<'U, 'S>) (r: Com<'V, 'S>) : Com<'U, 'S> =
  l *> v <* r

let sepBy1 (p: Com<'T, 'S>) (sep: Com<'U, 'S>) : Com<'T list, 'S> =
  p <+> many (sep *> p)
  |>> List.Cons

let sepBy (p: Com<'T, 'S>) (sep: Com<'U, 'S>) : Com<'T list, 'S> =
  sepBy1 p sep <|> just []

let satisfy (pred: 'T -> bool) : Com<'T, 'T> = com {
  let! next = look
  if pred next then
    let! _ = item
    return next
  else return! fail()
}

let check (pred: 'T -> bool) : Com<bool, 'T> = state {
  match! look with
  | Success v ->
    if pred v then
      return Success true
    else return Success false
  | Failure -> return Success false
}

let one (tar: 'T) : Com<'T, 'T> =
  satisfy ((=) tar)

let oneOf (lst: 'T list) : Com<'T, 'T> =
  satisfy (fun x -> List.contains x lst)

//let chainL (p: Com<'T>) (op: Com<'T -> 'T -> 'T>) : Com<'T> = com {
//  let! f = op
//  let! first = p 
//  let rec loop prev = state {
//    match! p with
//    | Success curr -> return! loop (f prev curr)
//    | Failure -> return Success prev
//  }
//  return! loop first
//}

type IntParserState =
  { Tokens: int list
    Line: int
    Column: int }
  interface CombinatorState<int> with
    member x.Peek = com {
      if x.Tokens.Length > 0 then
        return x.Tokens.[0]
      else
        return! fail()
    }
    member x.Item = com {
      if x.Tokens.Length > 0 then
        let res = x.Tokens.[0]
        let updated = { x with
                          Tokens = x.Tokens.[1..] }
        do! com.set (updated :> CombinatorState<int>)
        return res
      else
        return! fail()
    }

let test = {
  Tokens = [1; 2; 3]
  Line = 1
  Column = 1
}
