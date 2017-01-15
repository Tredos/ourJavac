type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod

type unop =
  | Uplus | Uminus

type const =
  | Float of float
  | Int of int


type basicType =
  | ByteType
  | ShortType
  | IntType
  | LongType
  | CharType
  | BooleanType
  | FloatType
  | DoubleType

type operation =
  | BooleanOperation
  | Bool of bool
  | Const of const
  | Var of string
  | Binop of binop * operation * operation
  | Unop of unop * operation

type ifStatement =
	| IfThenElse of operation * statement * statement
	| IfThen of operation * statement

and forStatement =
	| BasicFor of statement option * operation option * statement option * statement option
	| EnhancedFor

and switch_statement = Switch of operation * switch_case list

and switch_case = 
  | Default_case of statement
  | Normal_case of operation * statement 

and statement =
  | Declaration of basicType * string * operation option
  | IfStatement of ifStatement
  | ForStatement of forStatement
  |  While of operation * statement
  | DoWhile of operation  * statement
  | SwitchStatement of switch_statement

type expression =
	| Statement of statement


type ast = expression list



exception Unbound_variable of string

let get_op_u = function
  | Uplus -> fun x -> x
  | Uminus -> fun x -> -x


let string_of_op_u = function
  | Uplus -> "+"
  | Uminus -> "-"

let string_of_op_b = function
  | Badd -> "Badd"
  | Bsub -> "Bsub"
  | Bmul -> "Bmul"
  | Bdiv -> "/"
  | Bmod -> "%"


let string_of_const = function
  | Float n -> " FLOAT :"^(string_of_float n)^" "
  | Int n  -> " INT : "^(string_of_int n)^" "

let string_of_bool = function
  | true -> "BOOLEAN TRUE"
  | false -> "BOOLEAN FALSE"

let string_of_basicType = function
  | ByteType -> " ByteType "
  | ShortType -> " ShortType "
  | IntType -> " IntType "
  | LongType -> " LongType "
  | CharType -> " CharType "
  | BooleanType -> " BooleanType "
  | FloatType -> " FloatType "
  | DoubleType -> " DoubleType "

(*)
let rec eval env exp =
  match exp with
  | Const c -> c
  | Var v -> (try List.assoc v env with Not_found -> raise(Unbound_variable v))
  | Binop(op,e1,e2) -> (get_op_b op) (eval env e1) (eval env e2)
  | Unop(op,e) -> (get_op_u op) (eval env e)
*)



let rec string_of_operation op =
  match op with
  | Const c -> string_of_const c
  | Var v -> "VAR "^v
  | Binop(op, e1, e2) ->
      "(" ^(string_of_operation e1)^ (string_of_op_b op) ^(string_of_operation e2)^ ")"
  | Unop(op, e) -> "(" ^ (string_of_op_u op) ^(string_of_operation e)^ ")"
  | Bool b -> string_of_bool b

(*
and rec string_of_switch_case_list = function
  | [] -> ""
  | t::q -> begin match t with 
            | Default_case(s) -> " Default: "^print_statement(s)^" \n"^string_of_switch_case_list(q)
            | Normal_case(op, s) -> " case :"^string_of_operation(op)^" : "^print_statement(s)^" \n"^string_of_switch_case_list(q)
            end
*)

let print_variable = function
|(d,v) -> print_string ("DECLARATION ("^d ^ "," ^string_of_int v ^")")



let rec print_statement = function
    | Declaration(bt, id, Some(op)) -> "Declaration avec valeur "^string_of_basicType(bt)^id^string_of_operation(op)
    | Declaration(bt, id, None) -> "Declaration sans valeur "^string_of_basicType(bt)^id
    | While(op, st) -> "While ("^string_of_operation(op)^") {"^print_statement st ^ "}"
    | DoWhile(op, st) -> "do{ \n"^print_statement st ^ ";\n} while (" ^ string_of_operation(op)^"); \n"
  	| IfStatement(i) -> begin match i with
  			| IfThenElse(op, e1, e2) -> " IF "^string_of_operation(op)^" THEN "^print_statement(e1)^" ELSE "^print_statement(e2)
			| IfThen(op, e1) -> " IF "^string_of_operation(op)^" THEN "^print_statement(e1)
  			end
  	| ForStatement(f) -> begin match f with
  			| BasicFor(Some(forinit),Some(condition),Some(forupdate),Some(action)) -> " FOR (init : "^print_statement(forinit)^" condition : "^string_of_operation(condition)^" update : "^print_statement(forupdate)^" DO "^print_statement(action)
  			end 
  	|SwitchStatement(s) -> begin match s with
        | Switch(op,switch_case_list) ->" SWITCH ("^string_of_operation(op)^") \n"
          end
        

let rec print_expression = function
	| [] -> ""
	| i::t -> begin match i with
				| Statement(s) -> print_statement(s)^print_expression(t)
				end
        

			

let print_ast = function
   | i -> print_expression i


