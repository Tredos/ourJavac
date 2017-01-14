%{
    open AST
%}

%token EOF EQ SEMICOLON UNKNOWN INTID

%token <string> STRING
%token <int> INT
%start start
%type <AST.ast> start
%%
start:
    | p=expression * EOF { (p) }

expression:
| EQ  SEMICOLON{ Expression }
| SEMICOLON { Semicolon}
| EQ { Equal}
| i=INT {Integer (i)}
| INTID s=STRING EQ i = INT SEMICOLON { IntDeclaration (s,i)}
| s = STRING { String (s)}
| UNKNOWN  { Invalid}
