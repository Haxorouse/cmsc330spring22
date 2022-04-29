open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  if lookahead toks = Some Tok_Let then
    parseLetExpr toks
  else (if lookahead toks = Some Tok_If then
    parseIfExpr toks
  else (if lookahead toks = Some Tok_Fun then
    parseFunctionExpr toks
  else (*if lookahead toks = Some Tok_Or then*)
    parseOrExpr toks))
  (*else raise (ParseError "parse expr")*)
and parseLetExpr toks =
  let t1 = match_token toks Tok_Let in
  if lookahead toks = Some Tok_Rec then
    if lookahead t1 = Some (Tok_ID _) then
      let name = Some (lookahead t1) in
      let t2 = match_token t1 Tok_ID in
      let t3 = match_token t2 Tok_Equal in
      let o1 = parse_expr t3 in
      match o1 with (t4, e1) ->
      let t5 = match_token t4 Tok_In in
      let o2 = parse_expr t5 in
      match o2 with (t6, e2) ->
      t6 * Let((name:var), true, e1, e2)
    else raise (ParseError "parse Let")
  else if lookahead = Some Tok_ID then
    let name = Some (lookahead t1) in
    let t2 = match_token t1 Tok_ID in
    let t3 = match_token t2 Tok_Equal in
    let o1 = parse_expr t3 in
    match o1 with (t4, e1) ->
    let t5 = match_token t4 Tok_In in
    let o2 = parse_expr t5 in
    match o2 with (t6, e2) ->
    (*t6 * name = e1 in e2*)
    t6 * Let((name:var), false, e1, e2)
  else raise (ParseError "parse Let")
and parseFunctionExpr toks = 
  let t1 = match_token toks Tok_Fun in
  if lookahead t1 = Some Tok_ID then
    let name = Some (lookahead t1) in
    let t2 = match_token t1 Tok_ID in
    let t3 = match_token t2 Tok_Arrow in
    let o1 = parse_expr t3 in
    match o1 with (t4, e1) ->
    (*t4 * fun name -> e1*)
    t4 * Fun((name:var), e1)
  else raise (ParseError "parse fun")
and parseIfExpr toks = 
  let t1 = match_token toks Tok_If in
  let o1 = parse_expr t1 in
  match o1 with (t2, e1) ->
  let t3 = match_token t2 Tok_Then in
  let o2 = parse_expr t3 in
  match o2 with (t4, e2) ->
  let t5 = match_token t4 Tok_Else in
  let o3 = parse_expr t5 in
  match o3 with (t6, e3) ->
  (*t6 * if e1 then e2 else e3*)
  t6 * If(e1, e2, e3)
and parseOrExpr toks = 
  (*parse and expression, check look ahead for ||, if it's there match it and parse next or, otherwise done*)
  let o1 = parseAndExpr toks in
  match o1 with (t1, e1) ->
  if lookahead t1 = Some Tok_Or then
    let t2 = match_token t1 Tok_Or in
    let o2 = parseOrExpr t2 in
    match o2 with (t3, e2) ->
    (*t3 * e1 || e2*)
    t3 * Binop(Or, e1, e2)
  else o1
and parseAndExpr toks = 
  let o1 = parseEqualityExpr toks in
  match o1 with (t1, e1) ->
  if lookahead t1 = Some Tok_And then
    let t2 = match_token t1 Tok_And in
    let o2 = parseAndExpr t2 in
    match o2 with (t3, e2) ->
    (*t3 * e1 && e2*)
    t3 * Binop(And, e1, e2)
  else o1
and parseEqualityExpr toks =
  let o1 = parseRelationalExpr in
  match o1 with (t1, e1) ->
  if lookahead t1 = Some Tok_Equal then
    let t2 = match_token t1 Tok_Equal in
    let o2 = parseEqualityExpr t2 in
    match o2 with (t3, e2) ->
    (*t3 * e1 opp e2*)
    t3 * Binop(Equal, e1, e2)
  else if lookahead t1 = Some Tok_NotEqual then
    let t2 = match_token t1 Tok_NotEqual in
    let o2 = parseEqualityExpr t2 in
    match o2 with (t3, e2) ->
    t3 * Binop(NotEqual, e1, e2)
  else o1
and parseRelationalExpr toks =
  let o1 = parseAdditiveExpr in
  match o1 with (t1, e1) ->
  let opps = lookahead t1 in
  if opps = Some Tok_Greater then
    let t2 = match_token t1 Tok_Greater in
    let o2 = parseRelationalExpr t2 in
    match o2 with (t3, e2) ->
    (*t3 * e1 opp e2*)
    t3 * Binop(Greater, e1, e2)
  else if opps = Some Tok_LessEqual then
    let t2 = match_token t1 Tok_Less in
    let o2 = parseRelationalExpr t2 in
    match o2 with (t3, e2) ->
    t3 * Binop(Less, e1, e2)
  else if opps = Some Tok_GreaterEqual then
    let t2 = match_token t1 Tok_GreaterEqual in
    let o2 = parseRelationalExpr t2 in
    match o2 with (t3, e2) ->
    t3 * Binop(GreaterEqual, e1, e2)
  else if opps = Some Tok_Less then
    let t2 = match_token t1 Tok_LessEqual in
    let o2 = parseRelationalExpr t2 in
    match o2 with (t3, e2) ->
    t3 * Binop(LessEqual, e1, e2)
  else o1
and parseAdditiveExpr toks =
  let o1 = parseMultiplicativeExpr in
  match o1 with (t1, e1) ->
  if lookahead t1 = Some Tok_Add then
    let t2 = match_token t1 Tok_Add in
    let o2 = parseAdditiveExpr t2 in
    match o2 with (t3, e2) ->
    (*t3 * e1 opp e2*)
    t3 * Binop(Add, e1, e2)
  else if lookahead t1 = Some Tok_Sub then
    let t2 = match_token t1 Tok_Sub in
    let o2 = parseAdditiveExpr t2 in
    match o2 with (t3, e2) ->
    t3 * Binop(Sub, e1, e2)
  else o1
and parseMultiplicativeExpr toks =
  let o1 = parseConcatExpr in
  match o1 with (t1, e1) ->
  if lookahead t1 = Some Tok_Mult then
    let t2 = match_token t1 Tok_Mult in
    let o2 = parseMultiplicativeExpr t2 in
    match o2 with (t3, e2) ->
    (*t3 * e1 opp e2*)
    t3 * Binop(Mult, e1, e2)
  else if lookahead t1 = Some Tok_Div then
    let t2 = match_token t1 Tok_Div in
    let o2 = parseMultiplicativeExpr t2 in
    match o2 with (t3, e2) ->
    t3 * Binop(Div, e1, e2)
  else o1
and parseConcatExpr toks =
  let o1 = parseUnaryExpr in
  match o1 with (t1, e1) ->
  if (lookahead t1 = Some Tok_Concat) then
    let t2 = match_token t1 Tok_Concat in
    let o2 = parseConcatExpr t2 in
    match o2 with (t3, e2) ->
    (*t3 * e1 ^ e2*)
    t3 * Binop(Concat, e1, e2)
  else o1
and parseUnaryExpr toks =
  if lookahead toks = Some Tok_Not then
    let t1 = match_token toks Tok_Not in
    let o1 = parseUnaryExpr t1 in
    match o1 with (t2, e1) ->
    (*t2 * not e1*)
    t2 * Not(e1)
  else 
    parseFunctionCallExpr toks
and parseFunctionCallExpr toks =
  let o1 = parsePrimaryExpr toks in
  match o1 with (t1, e1) ->
  let opps = lookahead t1 in
  if (opps = Some Tok_Int) || (opps = Some Tok_Bool) || (opps = Some Tok_String) || (opps = Some Tok_ID) || (opps = Some Tok_RParen) then
    let opp = Some opps in
    let t2 = match_token t1 opp in
    let o2 = parsePrimaryExpr t2 in
    match o2 with (t3, e2) ->
    (*t3 * e1 e2*)
    t3 * FunctionCall(e1, e2)
  else o1
and parsePrimaryExpr toks =
  let opps = Some (lookahead toks) in
  if opps = Tok_Int then
    (*match opps with i ->*)
    let t1 = match_token toks Tok_Int in
    t1 * Value(Int(opps))
  else if opps = Tok_Bool then
    let t1 = match_token toks Tok_Bool in
    t1 * Value(Bool(opps))
  else if opps = Tok_String then
    let t1 = match_token toks Tok_String in
    t1 * Value(String(opps))
  else if opps = Tok_ID then
    let t1 = match_token toks Tok_ID in
    t1 * ID(opps:var)
  else if opps = Tok_RParen then
    let t1 = match_token toks Tok_RParen in
    let o1 = parse_expr t1 in
    match o1 with (t2, e1) ->
    let t3 = match_token t2 Tok_LParen in
    t3 * e1
  else raise (ParseError "bad match")

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = failwith "unimplemented"