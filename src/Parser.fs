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
  else
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

type ParserResult<'T> =
  | Success of 'T
  | Failure

type Com<'T> = ParserM<ParserResult<'T>>

let ( <|> ) (m1: Com<'T>) (m2: Com<'T>) : Com<'T> = state {
  match! m1 with
  | Success v -> return Success v
  | Failure -> return! m2
}

let ( <*> ) (f: Com<'T -> 'U>) (m: Com<'T>) : Com<'U> = state {
  let! a = f
  let! b = m
  match a, b with
  | Success a, Success b -> return Success (a b)
  | _ -> return Failure
}

let ( <&> ) (f: 'T -> 'U) (m: Com<'T>) : Com<'U> = state {
  match! m with
  | Success v -> return Success (f v)
  | Failure -> return Failure
}

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

let satisfy (pred: TokenType -> bool) : Com<TokenType>  = state {
  match! look with
  | Success v ->
    if pred v then
      let! _ = item
      return Success v
    else return Failure
  | Failure -> return Failure
}

let one (tar: TokenType) : Com<TokenType> = state {
  return! satisfy ((=) tar)
}

let oneOf (lst: TokenType list) : Com<TokenType> = state {
  return! satisfy (fun x -> List.contains x lst)
}

let number : Com<TokenType> = state {
  return! satisfy (fun x -> match x with Number _ -> true | _ -> false)
}

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

let test = {
  Line = 1
  Column = 1
  Tokens = Lexer.lex "+9"
}

let unaryOp op parseExpr : Com<Expr> =
  fun x -> UnaryExpr (op, x)
  <&> one op
  *> parseExpr Precedence.None
  
let boolNegateOp = unaryOp Bang
let numNegateOp = unaryOp Minus
let numIdentityOp = unaryOp Plus

let unaryExpr parseExpr : Com<Expr> =
  boolNegateOp parseExpr
  <|> numNegateOp parseExpr
  <|> numIdentityOp parseExpr

let binaryOp op parseExpr : Com<Expr> =

let numberExpr parseExpr : Com<Expr> =
  fun (Number n) -> NumberExpr n
  <&> number

let rec expr prec : Com<Expr> = state {
  return! unaryExpr expr <|> numberExpr expr
}

(expr Precedence.None) test |> printfn "%A"

// --- Expression parsing ---
let parseUnaryExpr parseExpr : ParserCombinator = state {
  let! op = advance
  let! expr = parseExpr Precedence.Unary
  return UnaryExpr (op, expr)
}

let parseIdentifierExpr parseExpr : ParserCombinator = state {
  let! (Identifier iden) = advance
  match! peek with
  | LeftParen ->
    do! eat LeftParen
    match! peek with
    | RightParen ->
      do! eat RightParen
      return CallExpr (iden, [])
    | _ ->
      let rec loop acc : ParserM<Expr list> = state {
        match! peek with
        | Comma ->
          do! eat Comma
          let! expr = parseExpr Precedence.None
          return! loop (expr :: acc)
        | _ -> return List.rev acc
      }
      let! first = parseExpr Precedence.None
      let! parms = loop [first]
      do! eat RightParen
      return CallExpr(iden, parms)
  | _ -> return VarGetExpr iden
}

let parseNumberExpr parseExpr : ParserCombinator = state {
  let! (Number n) = advance
  return NumberExpr n
}

let parseGroupExpr parseExpr : ParserCombinator = state {
  do! eat LeftParen
  let! expr = parseExpr Precedence.None
  do! eat RightParen
  return expr
}

let getPrefixParser token =
  match token with
  | Plus | Minus | Bang -> Some parseUnaryExpr
  | Identifier _ -> Some parseIdentifierExpr
  | Number _ -> Some parseNumberExpr
  | LeftParen -> Some parseGroupExpr
  | _ -> None

let parseBinaryExpr parseExpr left : ParserCombinator = state {
  let! op = advance
  let! right = parseExpr (getInfixPrecedence op)
  return BinaryExpr (left, op, right)
}

let getOtherfixParser token =
  match token with
  | Plus | Minus | Asterisk | Slash -> Some parseBinaryExpr
  | _ -> None

let rec parseExpr precedence = state {
  let! parser = get
  let! first = peek
  let! prefix = getPrefixParser first |> unwrap
  let! left = prefix parseExpr
  let rec loop (left: Expr) = state {
    let! cont = checkPred (fun x -> precedence < getInfixPrecedence x)
    if cont then
      let! next = peek
      let! otherfix = getOtherfixParser next |> unwrap
      let! left = otherfix parseExpr left
      return! loop left
    else return left
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
  i32 c = fib
  fib()
  fib(1)
  fib(1,2)
  println (5+2)
}
"
}

parse parser |> printfn "%A" 
