(* Allows users to encode a secret using Asmuth-Bloom's scheme from the terminal
 * by inputting the secret, the number of shares, and the critical number of 
 * shares as arguments to this bytecode file. The shares and base are printed to
 * the terminal window. *)

let (shares, base) = CRT.encode_string_asmuth (Sys.argv.(1)) 
  (int_of_string (Sys.argv.(2))) (int_of_string (Sys.argv.(3))) ;;

let string_shares = List.map (List.map CRT.eq_to_string) shares;; 

print_string "Shares: " ;;
List.map (List.map print_string) string_shares;;
print_string ("Base: " ^ Big_int.string_of_big_int base ^ "\n");;
