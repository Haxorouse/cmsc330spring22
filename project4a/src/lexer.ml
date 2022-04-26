open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let ReRParen = Str.regexp "("
let ReLParen = Str.regexp ")"
let ReEqu = Str.regexp "="
let ReNotEqu = Str.regexp "<>"
let ReGrtr = Str.regexp ">"
let ReLss = Str.regexp "<"
let ReGrEq = Str.regexp ">="
let ReLsEq = Str.regexp "<="
let ReOr = Str.regexp "||"
let ReAnd = Str.regexp "&&"
let ReNot = Str.regexp "not"
let ReIf = Str.regexp "if"
let ReThn = Str.regexp "then"
let ReEls = Str.regexp "else"
let ReAdd = Str.regexp "\\+"
let ReSub = Str.regexp "-"
let ReMult = Str.regexp "\\*"
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
let ReWht = Str.regexp "\s" (*should match all white space characters*)
let tokenize input = 
    let rec scan pos s =
    if pos >= String.length s then
        []
    else
        if Str.string_match ReWht s pos then
            scan (pos + 1) s
        else if Str.string_match ReRParen s pos then

        else if Str.string_match ReLParen s pos then

        else if Str.string_match ReEqu s pos then

        else if Str.string_match ReNotEqu s pos then

        else if Str.string_match ReGrtr s pos then 

        else if Str.string_match ReLss s pos then

        else if Str.string_match ReGrEq s pos then

        else if Str.string_match ReLsEq s pos then

        else if Str.string_match ReOr s pos then

        else if Str.string_match ReAnd s pos then

        else if Str.string_match ReNot s pos then

        else if Str.string_match ReIf s pos then

        else if Str.string_match ReThn s pos then

        else if Str.string_match ReEls s pos then

        else if Str.string_match ReAdd s pos then

        else if Str.string_match ReSub s pos then

        else if Str.string_match ReMult s pos then

        else if Str.string_match ReDiv s pos then

        else if Str.string_match ReCnct s pos then

        else if Str.string_match ReLet s pos then

        else if Str.string_match ReDef s pos then

        else if Str.string_match ReIn s pos then

        else if Str.string_match ReFun s pos then

        else if Str.string_match ReArw s pos then

        else if Str.string_match ReDubSmi s pos then

        else if Str.string_match ReBool s pos then

        else if Str.string_match ReInt s pos then

        else if Str.string_match ReStr s pos then

        else if Str.string_match ReID s pos then 

        else