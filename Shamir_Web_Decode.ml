(* Allows users to decode a set of shares from the terminal. The user must input
 * the x and y components one by one into the terminal as separate strings, and
 * For simplicity's sake, this program only works on secrets that do not need to
 * be split - parsing through command line arguments becomes very difficult once
 * the secret is split *)

let point_list = ref [] ;;
let odd = ref Big_int.zero_big_int ;;

let _ = for i = 0 to (Array.length Sys.argv) - 2 do 
  (if i mod 2 = 1 then odd := Big_int.big_int_of_string Sys.argv.(i) else 
   if i > 0 then 
     point_list := (!odd, Big_int.big_int_of_string Sys.argv.(i)) :: !point_list
   else ())
done  
;;

print_string ("Secret: " ^
  Helpers.big_int_to_string (Shamirs.decode_shamirs !point_list 
  (int_of_string Sys.argv.((Array.length Sys.argv) - 1)))) ;;
