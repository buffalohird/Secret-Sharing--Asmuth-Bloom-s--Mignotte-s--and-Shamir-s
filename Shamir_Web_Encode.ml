(* Allows users to encode a secret using Mignotte's scheme from the command line
 * by inputting the secret, the number of shares, and the critical number of 
 * shares as arguments to this bytecode file. The shares and base are printed to
 * the terminal window. *)

(* Allows users to encode a secret using Mignotte's scheme from the command line
 * by inputting the secret, the number of critical shares, and the number of 
 * shares as arguments to this bytecode file. The shares are printed to the 
 * terminal window *)

let shares = Shamirs.encode_shamirs (Helpers.string_to_big_int (Sys.argv.(1))) 
  (int_of_string (Sys.argv.(2))) (int_of_string (Sys.argv.(3))) ;;

let string_shares = List.map Helpers.point_to_string shares ;; 

print_string "Shares: " ;;
List.map print_string string_shares ;;
