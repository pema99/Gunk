module Parser
#nowarn "25"
#nowarn "40"

open LexerRepr
open ParserRepr
open Combinator
open Util
open State

// --- Expression combinators ---
let numberExpr : Com<Expr> =
  numberP
  |>> fun (Number n) -> NumberExpr n

let boolExpr : Com<Expr> =
  oneOf [True; False]
  |>> fun b -> match b with True -> BoolExpr true | False -> BoolExpr false

let stringExpr : Com<Expr> =
  stringP
  |>> fun (String s) -> StringExpr s

let valueExpr _ : Com<Expr> =
  numberExpr <|> boolExpr <|> stringExpr

let groupExpr expr : Com<Expr> =
  one LeftParen *> expr Precedence.None <* one RightParen

let unaryOp op parseExpr : Com<Expr> =
  fun x -> UnaryExpr (op, x)
  <&> one op
  *> parseExpr Precedence.None

let unaryExpr parseExpr : Com<Expr> =
  unaryOp Bang parseExpr
  <|> unaryOp Minus parseExpr
  <|> unaryOp Plus parseExpr

let binaryOp op parseExpr left : Com<Expr> =
  one op
  *> parseExpr (getInfixPrecedence op)
  |>> fun x -> BinaryExpr (left, op, x)

let binaryExpr parseExpr left : Com<Expr> =
  binaryOp Plus parseExpr left
  <|> binaryOp Minus parseExpr left
  <|> binaryOp Asterisk parseExpr left
  <|> binaryOp Slash parseExpr left

// --- Expression parsing ---
let getPrefixParser token : Com<_> = state {
  match token with
  | Plus | Minus | Bang -> return Success unaryExpr
  //| Identifier _ -> Some identifierExpr
  | Number _ | String _ | True | False -> return Success valueExpr
  | LeftParen -> return Success groupExpr
  | _ -> return Failure
}

let getOtherfixParser token : Com<_> = state {
  match token with
  | Plus | Minus | Asterisk | Slash -> return Success binaryExpr
  | _ -> return Failure
}



let rec parseExpr prec : Com<Expr> = com {
  let! first = look
  let! prefix = getPrefixParser first
  let! left = prefix parseExpr
  let rec loop (left: Expr) : Com<Expr> = com {
    let! cont = check (fun x -> prec < getInfixPrecedence x)
    if cont then
      let! next = look
      let! otherfix = getOtherfixParser next
      let! left = otherfix parseExpr left
      return! loop left
    else return left  
  }
  return! loop left
}

let test = {
  Line = 1
  Column = 1
  Tokens = Lexer.lex "5+9"
}

parseExpr Precedence.None test |> printfn "%A"
(*
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

parse parser |> printfn "%A" *)
