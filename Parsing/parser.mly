%{
    open AST
%}

%token EOF SEMICOLON PLUS MINUS DIV TIMES MOD  FALSE TRUE IDFLOAT IDINT IDBYTE IDSHORT IDLONG IDCHAR IDDOUBLE IDBOOLEAN IF THEN ELSE LBRA RBRA FOR  LPAR RPAR EQ
%token <string> IDENT
%token <string> STRING
%token <float> FLOAT
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
| IDINT s=STRING EQ i = INT SEMICOLON { IntDeclaration (s,i)}
| s = STRING { String (s)}
