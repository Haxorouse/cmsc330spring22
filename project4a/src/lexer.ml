open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let ReRParen = Str.regexp "("
let ReLParen = Str.regexp ")"
let ReEqu = Str.regexp "="
let ReNotEqu = Str.regexp "<>"
let ReGrtr = Str.regexp ">"
let ReLss = Str.regexp "<"
let ReGrEq = Str.regexp ">="
let ReGrLs = Str.regexp "<="
let ReOr = Str.regexp "||"
let ReAnd = Str.regexp "&&"
let ReNot = Str.regexp "not"
let ReIf = Str.regexp "if"
let ReThn = Str.regexp "then"
let ReEls = Str.regexp "else"
let ReAdd = Str.regexp "\\+"
let ReSub = Str.regexp "-"
let ReMult = Str.regexp "\*"
let ReDiv = Str.regexp "/"
let ReCnct = Str.regexp "\\^"
let ReLet = Str.regexp "let"
let ReDef = Str.regexp "def"
let ReIn = Str.regexp "in"
let ReFun = Str.regexp "fun"
let ReArw = Str.regexp "->"
let ReDubSmi = Str.regexp ";;"
let ReBool = Str.regexp "true|false"
let ReInt = Str.regexp "[0-9]+"
let ReStr = Str.regexp "\"[^\"]*\""
let ReID = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"

let tokenize input = 
    let rec scan pos s =
    if pos >= String.length s then
        []
    else
        if