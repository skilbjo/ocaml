
let print_hello () = print_endline "Hello world" ;;

(*Recursive function needs the word 'rec'*)
let rec fac n =
  if n = 0 then 1
  else n * fac (n - 1) ;;

let fac_builder n =
  "fac " ^ string_of_int(n) ^ " is: " ^ string_of_int(
  fac n ) ;;

(*This is a lambda*)
let fac_improver n =
  fun x -> fac n

let fac_builder2 n =
  fac_improver n

let fac47 =
  fac_builder 47 ;;

let fac48 =
  fac_builder 48 ;;

let fac49 =
  fac_builder 49 ;;

let rec is_even = function
  | 0 -> true
  | n -> is_odd (n-1)
and
  is_odd = function
    | 0 -> false
    | n -> is_even(n-1) ;;

let is_zero x =
  match x with
  | 0 -> true
  | _ -> false  ;;

let rec sum_list l =
    match l with
    | [] -> 0
    | head :: tail -> head + (sum_list tail) ;;

let my_list = [1; 2; 3] ;;

type int_list = Nil | Cons of int * int_list ;;

let rec sum_int_list l =
  match l with
  | Nil -> 0
  | Cons (head, tail) -> head + (sum_int_list tail) ;;

let t = Cons (1, Cons (2, Cons (3, Nil))) ;;
sum_int_list t ;;

let main() =
  print_endline fac48 ;;
  print_endline fac47 ;;
  print_endline fac49 ;;
  print_endline "check out this int overflow" ;;
  print_endline( string_of_bool( is_even(40) ) ) ;;
  print_endline( string_of_bool( is_zero(40) ) ) ;;
  print_endline( string_of_int( sum_list(my_list) ) ) ;;
  print_endline( string_of_int( sum_int_list(t) ) ) ;;

main()
