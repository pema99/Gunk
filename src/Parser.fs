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
let getPrefixParser token : Com<_> = com {
  match token with
  | Plus | Minus | Bang -> return unaryExpr
  //| Identifier _ -> Some identifierExpr
  | Number _ | String _ | True | False -> return valueExpr
  | LeftParen -> return groupExpr
  | _ -> return! fail()
}

let getOtherfixParser token : Com<_> = com {
  match token with
  | Plus | Minus | Asterisk | Slash -> return binaryExpr
  | _ -> return! fail()
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

// --- Statement parsing
//TODO: No false positives here
let parseVarSignature : Com<string * ExprType> =
  let typed = item >>= tokenToExprType
  let iden = identifierP |>> fun (Identifier n) -> n 
  typed <+> iden |>> flip 

let parseBlockStmt parseStmt : Com<Stmt> =
  (one LeftBrace *> many (parseStmt) <* one RightBrace)
  |>> BlockStmt

let parsePrintStmt parseExpr : Com<Stmt> =
  let printP t b =
    one t *> parseExpr Precedence.None
    |>> fun x -> PrintStmt(b, x)
  printP Print false <|> printP PrintLine true

let parseClearStmt : Com<Stmt> =
  one Clear |>> fun _ -> ClearStmt  

//parseTypedStmt

let rec parseStmt : Com<Stmt> = com {
  return! 
    parseBlockStmt parseStmt
    <|> parsePrintStmt parseExpr
    <|> parseClearStmt
    <|> (parseExpr Precedence.None |>> ExprStmt)
}

let test = {
  Tokens = Lexer.lex "{ 2 + 3 print 2 println 5 }"
  Line = 1
  Column = 1
}

parseStmt test |> printfn "%A"

(*
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
