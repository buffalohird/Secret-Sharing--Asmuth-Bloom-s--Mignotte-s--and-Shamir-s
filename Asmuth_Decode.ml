(* Allows users to decode a set of shares from the terminal. The user must input
 * the residues and moduli one by one into the terminal as separate strings, and
 * then input the base as the final argument. For simplicity's sake, this 
 * program only works on secrets that do not need to be split - parsing through
 * command line arguments becomes very difficult once the secret is split *)

let eq_list = ref [] ;;
let odd = ref "" ;;

let _ = for i = 0 to (Array.length Sys.argv) - 2 do 
  (if i mod 2 = 1 then odd := Sys.argv.(i) else 
   if i > 0 then 
     eq_list := (CRT.strings_to_eq !odd Sys.argv.(i)) :: !eq_list
   else ())
done  
;;


print_string ("Secret: " ^ (CRT.decode_string_asmuth [!eq_list] 
  (Big_int.big_int_of_string (Sys.argv.((Array.length Sys.argv) - 1))))^ "\n");
