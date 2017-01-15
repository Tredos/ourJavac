%{
    open AST
%}

%token EOF SEMICOLON PLUS MINUS DIV TIMES MOD  FALSE TRUE IDFLOAT IDINT IDBYTE IDSHORT IDLONG IDCHAR IDDOUBLE IDBOOLEAN IF THEN ELSE LBRA RBRA FOR  LPAR RPAR
/*assignment Operators*/
%token EQ SELFADD SELFSUB SELFMUL SELFDIV SELFAND SELFOR SELFXOR SELFMOD SELFLEFTSHIFT SELFRIGHTSHIFT USELFRIGHTSHIFT

%token  INCREMENT DECREMENT NEGATION BCOMPLEMENT BREAK SWITCH CASE DEFAULT COLON

%token <string> IDENT
%token <string> STRING
%token <float> FLOAT
%token <int> INT
%start start
%type <AST.ast> start
%type < AST.expression > expression
%type < AST.statement > statement
%type  <AST.operation > operation
%type < AST.const > const




%left PLUS MINUS
%left TIMES DIV MOD
%right UMINUS UPLUS

%%
start:
    | p=expression * EOF { (p) }

expression:
	| s=statement 		{ Statement(s)}

statement:
  | d=declaration {d}
  | i=ifStatement {i}
  | f=forStatement {f}
  | s=switchStatement {s}

prefix_operator:
  | NEGATION {"!"}
  | BCOMPLEMENT {"~"}

postfix_operator:
  | INCREMENT {"++"}
  | DECREMENT   {"--"}

switchStatement:
	| SWITCH LPAR op=operation RPAR LBRA b=switch_case_group RBRA
		{ SwitchStatement(Switch(op, b))}

switch_case_group:
	| s=switch_case {[s]}
	| s=switch_case b=switch_case_group {s::b}

switch_case:
	| CASE op=operation COLON s=statement BREAK SEMICOLON {Normal_case(op,s)}
	| DEFAULT COLON s=statement 						  {Default_case(s)}


ifStatement:
  | IF LPAR op=operation RPAR LBRA e=statement RBRA ELSE LBRA e2=statement RBRA 
            { IfStatement(IfThenElse(op,e,e2)) }
  | IF LPAR op=operation RPAR LBRA e=statement RBRA                        
            { IfStatement(IfThen(op,e)) }

assignment_operator:
  | EQ {"="}
  | SELFADD {"+="}
  | SELFSUB {"-="}
  | SELFMUL {"*="}
  | SELFDIV {"/="}
  | SELFAND {"&="}
  | SELFOR  {"|="}
  | SELFXOR {"^="}
  | SELFMOD {"%="}
  | SELFLEFTSHIFT {"<<="}
  | SELFRIGHTSHIFT {">>="}
  | USELFRIGHTSHIFT  {">>>="}

forStatement:
  | FOR LPAR forinit=statement condition=operation SEMICOLON forupdate=statement RPAR LBRA action=statement RBRA
            { ForStatement(BasicFor(Some(forinit),Some(condition),Some(forupdate),Some(action)))}

declaration :
  | i=basicType id=IDENT EQ o=operation SEMICOLON { Declaration(i,id,Some(o))}
  | i=basicType id=IDENT SEMICOLON                  { Declaration(i,id, None)}

operation:
  | TRUE
      { Bool true}
  | FALSE
      { Bool false}
  | LPAR e=operation RPAR
      { e }
  | MINUS e=operation %prec UMINUS
      { Unop(Uminus,e)}
  | PLUS e=operation %prec UPLUS
      { Unop(Uplus,e)}
  | e1=operation o=bop e2=operation
      { Binop(o,e1,e2)}
  | id=IDENT
      { Var id }
  | c=const 
      {Const c}



const:
  | i=INT {Int i}
  | f=FLOAT {Float f}

(*
| EQ  SEMICOLON{ Expression }


| SEMICOLON { Semicolon}
| EQ { Equal}
| i=INT {Integer (i)}
| IDINT s=STRING EQ i = INT SEMICOLON { IntDeclaration (s,i)}
| s = STRING { String (s)}
*)


%inline bop:
  | MINUS     { Bsub }
  | PLUS      { Badd }
  | TIMES     { Bmul }
  | DIV       { Bdiv }
  | MOD       { Bmod }

%inline basicType:
  | IDBYTE    { ByteType }
  | IDSHORT   { ShortType }
  | IDINT     { IntType }
  | IDLONG    { LongType }  
  | IDCHAR    { CharType }
  | IDFLOAT   { FloatType }
  | IDDOUBLE   { DoubleType }
  | IDBOOLEAN   { BooleanType }
 
%%