module Parser
#nowarn "25"
#nowarn "40"

open LexerRepr
open ParserRepr
open Combinator
open Util

// --- Expression combinators ---
let identifierP : Parser<TokenType> =
  satisfy (fun x -> match x with Identifier _ -> true | _ -> false)

let numberExpr : Parser<Expr> =
  satisfy (fun x -> match x with Number _ -> true | _ -> false)
  |>> fun (Number n) -> NumberExpr n

let boolExpr : Parser<Expr> =
  oneOf [True; False]
  |>> fun b -> match b with True -> BoolExpr true | False -> BoolExpr false

let stringExpr : Parser<Expr> =
  satisfy (fun x -> match x with String _ -> true | _ -> false)
  |>> fun (String s) -> StringExpr s

let valueExpr _ : Parser<Expr> =
  numberExpr <|> boolExpr <|> stringExpr

let groupExpr parseExpr : Parser<Expr> =
  one LeftParen *> parseExpr Precedence.None <* one RightParen

let unaryOp op parseExpr : Parser<Expr> =
  fun x -> UnaryExpr (op, x)
  <!> one op
  *> parseExpr Precedence.None

let unaryExpr parseExpr : Parser<Expr> =
  unaryOp Bang parseExpr
  <|> unaryOp Minus parseExpr
  <|> unaryOp Plus parseExpr

let binaryOp op parseExpr left : Parser<Expr> =
  one op
  *> parseExpr (getInfixPrecedence op)
  |>> fun x -> BinaryExpr (left, op, x)

let binaryExpr parseExpr left : Parser<Expr> =
  binaryOp Plus parseExpr left
  <|> binaryOp Minus parseExpr left
  <|> binaryOp Asterisk parseExpr left
  <|> binaryOp Slash parseExpr left

// --- Expression parsing ---
let getPrefixParser token : Parser<_> = com {
  match token with
  | Plus | Minus | Bang -> return unaryExpr
//| Identifier _ -> Some identifierExpr
  | Number _ | String _ | True | False -> return valueExpr
  | LeftParen -> return groupExpr
  | _ -> return! fail()
}

let getOtherfixParser token : Parser<_> = com {
  match token with
  | Plus | Minus | Asterisk | Slash -> return binaryExpr
  | _ -> return! fail()
}

let rec parseExpr prec : Parser<Expr> = com {
  let! first = look
  let! prefix = getPrefixParser first
  let! left = prefix parseExpr
  let rec loop (left: Expr) : Parser<Expr> = com {
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

// --- Statement parsing ---
let parseVarSignature : Parser<string * ExprType> =
  let typeTokens = [
    TokenType.U8; TokenType.U16; TokenType.U32; TokenType.U64
    TokenType.I8; TokenType.I16; TokenType.I32; TokenType.I64
    TokenType.F32; TokenType.F64; TokenType.Str; TokenType.Bool
    TokenType.Unit
  ]
  let typed = (oneOf typeTokens <|> identifierP) >>= tokenToExprType
  let iden = identifierP |>> fun (Identifier n) -> n 
  typed <+> iden |>> flip 

let parseBlock parseStmt : Parser<Stmt list> =
  one LeftBrace *> many parseStmt <* one RightBrace

let parseBlockStmt parseStmt : Parser<Stmt> =
  BlockStmt <!> parseBlock parseStmt

let parsePrintStmt parseExpr : Parser<Stmt> =
  let printP t b =
    one t *> parseExpr Precedence.None
    |>> fun x -> PrintStmt(b, x)
  printP Print false <|> printP PrintLine true

let parseClearStmt : Parser<Stmt> =
  one Clear |>> fun _ -> ClearStmt  

let parseTypedStmt parseExpr parseStmt : Parser<Stmt> =
  let variableP (name, typed) =
    one Equal
    *> parseExpr Precedence.None
    |>> fun x -> VarDeclStmt (name, typed, x)
  let funcP (name, typed) =
    let parmsP =
      between
        (one LeftParen)
        (sepBy parseVarSignature (one Comma))
        (one RightParen)
    parmsP <+> parseBlock parseStmt
    |>> fun (parms, body) -> FuncDeclStmt (name, typed, parms, body) 
  parseVarSignature >>= fun x -> variableP x <|> funcP x

let rec parseStmt : Parser<Stmt> = com {
  return! 
    parseBlockStmt parseStmt
    <|> parseTypedStmt parseExpr parseStmt
    <|> parsePrintStmt parseExpr
    <|> parseClearStmt
    <|> (parseExpr Precedence.None |>> ExprStmt)
}

let parseP = many parseStmt

let parse tokens = 
  let init = {
    Tokens = tokens
    Line = 1
    Column = 1
  }
  parseP init |> fst

//Test
let tokens = Lexer.lex "
i8 a = 2 + 3
u8 b = 3 * 2
i32 fib (i32 lmao, u32 test) {
  i32 c = 2
  2 + 5
  println (5+2)
}"

match tokens with
| Success v -> parse v |> printfn "%A"
| _ -> printfn "Lexer failure"

(*
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
*)

//TODO:
//Parse function calls
//Parse identifier expressions