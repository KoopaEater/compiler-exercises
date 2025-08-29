(* Defining the type for binary operations *)
type binop = Add | Sub | Mul | Div

(* Defining the type for arithmetic expressions *)
type expr 
  = Int of int                       (* Integer constant *)
  | BinOp of binop * expr * expr     (* Binary operation *)

let string_of_binop binop = match binop with
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"

(* PRINTING AST *)

let rec string_of_indent indent last = match indent with
  | n when n > 1 -> "    " ^ string_of_indent (n-1) last
  | n when n = 1 -> if last then "└── " else "├── "
  | _ -> ""

let rec string_of_expr_helper (expr : expr) (indent : int) = match expr with
  | Int n ->
    "Int " ^ string_of_int n
  | BinOp (binop, expr1, expr2) ->
    "Binop " ^ string_of_binop binop ^ "\n" ^
    (string_of_indent (indent + 1) false) ^ (string_of_expr_helper expr1 (indent + 1)) ^ "\n" ^
    (string_of_indent (indent + 1) true) ^ (string_of_expr_helper expr2 (indent + 1))

let string_of_expr expr = string_of_expr_helper expr 0
let print_tree expr = print_endline (string_of_expr expr)

let () = print_tree (BinOp (Add, Int 1, BinOp (Mul,  Int 2, Int 3)))

(* PRINTING DOT*)

let rec dot_string_of_expr_helper expr index = match expr with
  | Int i -> "  " ^(string_of_int index) ^ " [label=\"Int " ^ (string_of_int i) ^ "\"];", (index + 1)
  | BinOp (binop, expr1, expr2) -> 
    let (left_tree, right_index) = dot_string_of_expr_helper expr1 (index+1) in
    let (right_tree, next_index) = dot_string_of_expr_helper expr2 (right_index) in
    "  " ^ (string_of_int index) ^ " [label=\"" ^ (string_of_binop binop) ^ "\"];\n" ^
    left_tree ^ "\n" ^
    right_tree ^ "\n" ^
    "  " ^ (string_of_int index) ^ " -> " ^ (string_of_int (index + 1)) ^ ";\n" ^
    "  " ^ (string_of_int index) ^ " -> " ^ (string_of_int (right_index)) ^ ";", (next_index)

let dot_string_of_expr expr = let str, _ = dot_string_of_expr_helper expr 0 in "digraph AST {\n" ^ str ^ "\n}"
let print_aexp_to_dot expr = print_endline (dot_string_of_expr expr)

let () = print_aexp_to_dot (BinOp (Add, BinOp (Mul,  Int 2, BinOp (Div, Int 16, Int 4)), Int 1))