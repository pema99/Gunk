module Parser
#nowarn "25"
#nowarn "40"

open LexerRepr
open ParserRepr
open Util
open State

// --- Helper functions ---
let fail : ParserM<unit> = state {
  let! parser = get
  raise <| ParserException ("Unexpected token", (parser.Line, parser.Column))
}

let unwrap operand : ParserM<'a> = state {
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
let advance : ParserM<TokenType> = state {
  let! parser = get
  let res = parser.Tokens.[0]
  do! set { parser with 
              Tokens = parser.Tokens.[1..]
              Line = res.Line
              Column = res.Column }
  return res.Type
}

//Advance 1 character, ignore it
let advanceIgnore : ParserM<unit> = state {
  let! _ = advance
  ()
}

//Peek the next character
let peek : ParserM<TokenType> = state {
  let! parser = get
  return parser.Tokens.[0].Type
}

//Match next character
let checkPred pred : ParserM<bool> = state {
  let! atEnd = isAtEnd
  if atEnd then
    return false
  else
    let! next = peek
    return pred next
}

//Match next characer with character
let check expected : ParserM<bool> = state {
  return! checkPred ((=) expected)
}

//Match next character and ignore it, throw if not valid
let eat expected : ParserM<unit> = state {
  let! next = peek
  if next = expected then
    do! advanceIgnore
  else
    do! fail
}

// --- Expression parsing ---
let parseUnaryExpr parseExpr : Combinator<Expr> = state {
  match! peek with
  | Minus | Plus | Bang ->
    let! op = advance
    let! expr = parseExpr Precedence.Unary
    return UnaryExpr (op, expr) |> Success
  | _ -> return Failure
}

let parseNumberExpr parseExpr : Combinator<Expr> = state {
  match! peek with
  | Number _ ->
    let! (Number n) = advance
    return NumberExpr n |> Success
  | _ -> return Failure
}

let parseGroupExpr parseExpr : Combinator<Expr> = state {
  match! peek with
  | LeftParen ->
    do! advanceIgnore
    let! expr = parseExpr Precedence.None
    match! check RightParen with
    | true -> return Success expr
    | false -> return FailureWith "Unmatched paranthesis"
  | _ -> return Failure
}

let getPrefixParser token =
  match token with
  | Plus | Minus | Bang -> Some parseUnaryExpr
  //| Identifier _ -> Some parseName
  | Number _ -> Some parseNumberExpr
  | LeftParen -> Some parseGroupExpr
  | _ -> None

let combineFailures a b =
  match a, b with
  | Failure, CompoundFailure _ | Failure, FailureWith _
  | Success _, CompoundFailure _ | Success _, FailureWith _ -> b
  | CompoundFailure _, Failure | FailureWith _, Failure
  | CompoundFailure _, Success _ | FailureWith _, Success _ -> a
  | FailureWith msg, CompoundFailure lst
  | CompoundFailure lst, FailureWith msg -> CompoundFailure (msg :: lst)
  | FailureWith l, FailureWith r -> CompoundFailure [l; r]
  | CompoundFailure l, CompoundFailure r -> CompoundFailure (l @ r)
  | _ -> Failure

let ( >>= ) (m: Combinator<'T>) (f: 'T -> ParserResult<'T>) : Combinator<'T> =
  fun s ->
    let a, b = m s
    match a with
    | Success v -> f v, b  
    | _ -> a, b

let ( <* ) (m1: Combinator<'T>) (m2: Combinator<'T>) : Combinator<'T> =
  fun s ->
    let a1, b1 = m1 s
    let a2, b2 = m2 b1
    match a1, a2 with
    | Success _, Success _ -> a1, b2
    | _ -> combineFailures a1 a2, b2
  
let ( *> ) a b =
  b <* a

let ( <|> ) (m1: Combinator<'T>) (m2: Combinator<'T>) : Combinator<'T> =
  fun s ->
    let a1, b1 = m1 s
    match a1 with
    | Success _ | FailureWith _ | CompoundFailure _ -> a1, b1
    | Failure -> m2 b1
      
let parseBinaryExpr parseExpr left : Combinator<Expr> = state {
  match! peek with
  | Plus | Minus | Asterisk | Slash ->
    let! op = advance
    let right = parseExpr (getInfixPrecedence op)
    return! (right >>= (fun x -> BinaryExpr (left, op, x) |> Success)) 
  | _ -> return Failure
}

let getOtherfixParser token =
  match token with
  | Plus | Minus | Asterisk | Slash -> Some parseBinaryExpr
  | _ -> None

let rec parseExpr precedence = state {
  let! parser = get
  let! first = peek
  let! left =
    parseUnaryExpr parseExpr
    <|> parseNumberExpr parseExpr
    <|> parseGroupExpr parseExpr
  let rec loop (left: ParserResult<Expr>) = state {
    match left with
    | Success left ->
      let! cont = checkPred (fun x -> precedence < getInfixPrecedence x)
      if cont then
        let! next = peek
        let! left = parseBinaryExpr parseExpr left
        return! loop left
      else return left
    | _ -> return left
  }
  return! loop left
}

// --- Statement parsing ---
let parseVarSignature : ParserM<string * ExprType> = state {
  //Type, identifier
  let! typed = advance
  let! typed = typed |> tokenToExprType |> unwrap
  let! iden = advance
  let nameOpt =
    match iden with
    | Identifier iden -> Some iden
    | _ -> None
  let! name = unwrap nameOpt
  return name, typed
}

let parseBlock parseStmt : ParserM<Stmt list> = state {
  do! eat LeftBrace
  let rec loop acc = state {
    let! atEnd = check RightBrace
    if atEnd then
      return acc
    else
      let! stmt = parseStmt
      return! loop (stmt :: acc)
  }
  let! body = loop []
  do! eat RightBrace
  return List.rev body
}

let parseBlockStmt parseExpr parseStmt : ParserM<Stmt> = state {
  let! block = parseBlock parseStmt
  return BlockStmt block
}

let parseTypedStmt parseExpr parseStmt : ParserM<Stmt> = state {
  let! name, typed = parseVarSignature
  let! next = advance
  match next with
  | Equal ->
    //Variable
    let! expr = parseExpr Precedence.None
    return VarDeclStmt (name, typed, expr)
  | LeftParen ->
    //Param list
    let rec loop acc = state {
      let! atEnd = check RightParen
      if atEnd then
        return acc
      else
        let! name, typed = parseVarSignature
        let! hasComma = check Comma
        if hasComma then
          do! eat Comma
          return! loop ((name, typed) :: acc)
        else
          return (name, typed) :: acc
    }
    let! parms = loop []
    do! eat RightParen
    let! body = parseBlock parseStmt
    return FuncDeclStmt (name, typed, List.rev parms, body)
}

let parsePrintStmt parseExpr parseStmt : ParserM<Stmt> = state {
  let! next = advance
  let! expr = parseExpr Precedence.None
  match next with
  | Print -> return PrintStmt (false, expr)
  | PrintLine -> return PrintStmt (true, expr)
}

let parseClearStmt parseExpr parseStmt : ParserM<Stmt> = state {
  do! eat Clear
  return ClearStmt
}

let getStmtParser token =
  match token with
  | TokenType.U8   | TokenType.U16 | TokenType.U32
  | TokenType.U64  | TokenType.I8  | TokenType.I16
  | TokenType.I32  | TokenType.I64 | TokenType.Unit
  | TokenType.Bool | TokenType.F32 | TokenType.F64 -> Some parseTypedStmt
  | TokenType.LeftBrace -> Some parseBlockStmt
  | TokenType.Print | TokenType.PrintLine -> Some parsePrintStmt
  | TokenType.Clear -> Some parseClearStmt
  | _ -> None

let rec parseStmt = state {
  let! parse = get
  let! first = peek
  let stmt = getStmtParser first
  match stmt with
  | Some stmt ->
    return! stmt parseExpr parseStmt
  | None ->
    let! expr = parseExpr Precedence.None
    return ExprStmt expr
}

// --- Parser ---
let parse = state {
  let rec loop acc = state {
    let! atEnd = isAtEnd
    if not atEnd then
      let! stmt = parseStmt
      return! loop (stmt :: acc) 
    else
      return List.rev acc
  }
  return! loop []
}

let parser = {
  Line = 1
  Column = 1
  Tokens = Lexer.lex "
i8 a = 2 + 3
u8 b = 3 * 2
i32 fib(i32 lmao, u32 test) {
  i32 c = 2
  2 + 5
  println (5+2)
}
"
}

parse parser |> printfn "%A" 
