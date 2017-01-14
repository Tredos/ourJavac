{
    open Parser
    open Location
    exception SyntaxError of string
}

let sub = '\x1A'
let space = [' ' '\t' ]
let letter = ['a'-'z' 'A'-'Z']
let name  = ['a'-'z''A'-'Z']+
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let intiger = digit+
let float = digit+ '.' digit*

rule read = parse
    | space+            { read lexbuf }
    | '\n'              { Location.incr_line lexbuf; read lexbuf }
    | "int" {IDINT}
    | name  { STRING (Lexing.lexeme lexbuf) }
    | intiger as nb {INT (int_of_string (nb))}
    | "=" {EQ}
    | ";" {SEMICOLON}
    | "+"           { PLUS }
    | "-"           { MINUS }
    | "/"           { DIV }
    | "*"           { TIMES }
    | "%"           { MOD }
    | ";"           { SEMICOLON }
    | intiger as nb     { INT (int_of_string nb) }
    | float as nb   { FLOAT (float_of_string nb)}
    | "false"       { FALSE }
    | "true"        { TRUE }
    | "float"       { IDFLOAT }
    | "int"         { IDINT }
    | "byte"        { IDBYTE }
    | "short"       { IDSHORT }
    | "long"        { IDLONG }
    | "char"        { IDCHAR }
    | "double"      { IDDOUBLE }
    | "boolean"     { IDBOOLEAN }
    | "if"          { IF }
    | "then"        { THEN }
    | "else"        { ELSE }
    | "{"           { LBRA }
    | "}"           { RBRA }
    | "for"         { FOR }
    | ident         { IDENT (Lexing.lexeme lexbuf) }
    | "("           { LPAR }
    | ")"           { RPAR }
    | "="           { EGAL }
    | eof {EOF}
