let () = print_endline "DATE FUN"

let valid_date d m = match d with
  | d when d < 1 -> false
  | _ -> match m with
    | "Jan" | "Mar" | "May" | "Jul" | "Aug" | "Oct" | "Dec" -> d < 31
    | "Apr" | "Jun" | "Sep" | "Nov" -> d < 30
    | "Feb" -> d < 28
    | _ -> false

let () = print_endline (string_of_bool (valid_date 29 "May"))