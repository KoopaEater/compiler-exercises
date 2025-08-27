(*
JUST PRINT HELPER FUNCTIONS
*)
let rec string_of_int_list_values = function
  | [] -> ""
  | x::xs -> " " ^ string_of_int x ^ " " ^ string_of_int_list_values xs
let string_of_int_list list = "[" ^ string_of_int_list_values list ^ "]"

let rec string_of_int_list_list_values = function
  | [] -> ""
  | x::xs -> " " ^ string_of_int_list x ^ " " ^ string_of_int_list_list_values xs
let string_of_int_list_list list = "[" ^ string_of_int_list_list_values list ^ "]"
(*
PRINT HELPER FUNCTIONS END HERE!
*)

(*
POWERSET FUNCTIONS
*)
let rec add_value_to_powerset (v : int) (p : int list list) = match p with
  | [] -> []
  | x::xs -> x::[v::x] @ add_value_to_powerset v xs

let rec powerset_helper acc rest = match rest with
  | [] -> acc
  | x::xs -> powerset_helper (add_value_to_powerset x acc) xs

let powerset set = powerset_helper [[]] set
(*
POWERSET FUNCTIONS END HERE!
*)

(*
MAIN
*)
let () = print_endline (string_of_int_list_list [[1] ; [2 ; 3] ; [4 ; 5 ; 6] ; [7]])
let p3 = powerset [0 ; 1 ; 2 ; 3]
let () = print_endline (string_of_int_list_list p3)