(* -- Use this in your solution without modifications -- *)
(* Defining the type for binary operations *)
type binop = Add | Sub | Mul | Div

(* Defining the type for arithmetic expressions *)
type expr 
  = Int of int                       (* Integer constant *)
  | BinOp of binop * expr * expr     (* Binary operation *)

let expression_01 = Int 5                                         (* 5       *)
let expression_02 = BinOp (Add, Int 1, expression_01)             (* 1+5     *)
let expression_03 = BinOp (Mul, BinOp (Add, Int 2, Int 2), Int 2) (* (2+2)*2 *)
let expression_04 = BinOp (Add, Int 2, BinOp (Mul, Int 2, Int 2)) (* 2+2*2   *)

let apply_binop = function
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )
  | Div -> ( / )
let rec eval = function
  | Int n -> n
  | BinOp (binop, expr1, expr2) -> apply_binop binop (eval expr1) (eval expr2)

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
let rec string_of_expr = function
  | Int n -> string_of_int n
  | BinOp (binop, expr1, expr2) -> "(" ^ (string_of_expr expr1) ^ " " ^ (string_of_binop binop) ^ " " ^ (string_of_expr expr2) ^ ")"

let _ = 
let expressions = [ expression_01 ; expression_02 ; expression_03 ; expression_04 ] in
let print_expr e = Printf.printf ("%s = %d\n") (string_of_expr e) (eval e) in 
List.map print_expr expressions