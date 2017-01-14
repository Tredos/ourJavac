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
let floating = digit+ '.' digit*

rule read = parse
    | space+            { read lexbuf }
    | '\n'              { Location.incr_line lexbuf; read lexbuf }
    | intiger as nb {INT (int_of_string (nb))}
    | floating as nb   { FLOAT (float_of_string nb)}
    | ";" {SEMICOLON}
    | "+"           { PLUS }
    | "-"           { MINUS }
    | "/"           { DIV }
    | "*"           { TIMES }
    | "%"           { MOD }
    | ";"           { SEMICOLON }

    | "="           {EQ}
    | "+="          {SELFADD}
    | "-="          {SELFSUB}
    | "*="          {SELFMUL}
    | "/="          {SELFDIV}
    | "&="          {SELFAND}
    | "|="          {SELFOR}
    | "^="          {SELFXOR}
    | "%="          {SELFMOD}
    | "<<="         {SELFLEFTSHIFT}
    | ">>="         {SELFRIGHTSHIFT}
    | ">>>="        {USELFRIGHTSHIFT}

    | "++"        {INCREMENT}
    | "--"        {DECREMENT}
    | "!"       {NEGATION}
    | "~"       {BCOMPLEMENT}


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
    | name  { STRING (Lexing.lexeme lexbuf) }
    | "("           { LPAR }
    | ")"           { RPAR }
    | eof {EOF}
