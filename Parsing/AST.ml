
type expression =
| Expression
| Invalid
| Equal
| Semicolon
| String of string
| Integer of int
| IntDeclaration of string * int

type ast = expression list

let print_variable = function
|(d,v) -> print_string ("DECLARATION ("^d ^ "," ^string_of_int v ^")")

let rec print_expression = function
    | [] -> ()
    | i::t -> begin match i with
            | Expression -> print_string "Expression!! "
            | Invalid -> print_string "invalid!! "
            | Semicolon -> print_string "semicolon!!"
            | Equal -> print_string "equal!! "
            | String s -> print_string("STRING("^s^")" )
            | Integer i -> print_string (string_of_int i)
            | IntDeclaration (d,v) -> print_string ("DECLARATION ("^d ^ "," ^string_of_int v ^")")
            end;
            print_expression t

let print_ast = function
   | i -> print_expression i
