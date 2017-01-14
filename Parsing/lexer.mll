{
    open Parser
    open Location
    exception SyntaxError of string
}

let sub = '\x1A'
let space = [' ' '\t' ]
let name  = ['a'-'z''A'-'Z']+
let digit = ['0'-'9']
let intiger = digit+

rule read = parse
    | space+            { read lexbuf }
    | '\n'              { Location.incr_line lexbuf; read lexbuf }
    | "int" {INTID}
    | name  { STRING (Lexing.lexeme lexbuf) }
    | intiger as nb {INT (int_of_string (nb))}
    | "=" {EQ}
    | ";" {SEMICOLON}
    | _ {UNKNOWN}
    | eof {EOF}
