open Nat ;;
open Big_int ;;

type eq = {res: big_int; modulus: big_int}
let base = big_int_of_int 255 ;; 
let total = ref zero_big_int ;;
let current = ref zero_big_int ;; 

(* converts lists of big_ints to lists of ints *)
let list_to_int lst = List.map (fun b -> int_of_big_int b) lst
;;
		
(* converts lists of ints to lists of big_ints *)
let list_to_big lst = List.map (fun i -> big_int_of_int i) lst
;;

(* calculates a unique big_int representation of a string using ASCII values *)
let string_to_big_int (str: string) : big_int = 
  String.iter (fun x -> total := add_int_big_int (Char.code x) 
    (mult_big_int base !total)) str;
  current := !total;
  total := zero_big_int;
  !current
;;

(* creates a string from a big_int based off the method above *)
let big_int_to_string (big: big_int) : string = 
  let rec big_int_to_string_list (b : big_int) : string list = 
    if lt_big_int b base then [String.make 1 (Char.chr (int_of_big_int b))]
    else let (q, r) = quomod_big_int b base in 
      (String.make 1 (Char.chr (int_of_big_int r))) :: 
      (big_int_to_string_list q) in
  String.concat "" (List.rev (big_int_to_string_list big))
;;

assert(big_int_to_string (string_to_big_int "asd") = "asd") ;;
assert(big_int_to_string (string_to_big_int "whee") = "whee") ;;
      
let point_to_string (p: big_int * big_int) : string = 
  let (x, y) = p in 
  "(" ^ (string_of_big_int x) ^ "," ^ (string_of_big_int y) ^ ")" ^ "\n"
;;


