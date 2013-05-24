open Nat ;;
open Big_int ;;
open Lazy ;;

type eq = {res: big_int; modulus: big_int}
type 'a stream = Cons of 'a * 'a stream Lazy.t

(* shift is the index of the starting prime modulus used
 * asmuth_index is the index of the prime used to split secrets
 *   in asmuth encryption
 * low values will cause mod_generator to fail
 * large values will increase runtime *)
let shift_index = 200 ;;
let asmuth_index = 100 ;;

let eq_to_string (eq: eq) : string = 
  let {res; modulus} = eq in 
  "(" ^ (string_of_big_int res) ^ "," ^ (string_of_big_int modulus) ^ ")" ^ "\n"
;;

let strings_to_eq (r: string) (m: string) : eq = 
  {res = big_int_of_string r; modulus = big_int_of_string m}
;;

(* computes the partial product of the first i numbers in a list *)
let rec partial_prod lst i = 
  if i > 0 then match lst with
    | h :: t -> mult_big_int h (partial_prod t (i - 1))
    | [] -> unit_big_int
  else unit_big_int
;;

(* helps test the function partial_prod *)
let test_partial_prod lst i = int_of_big_int 
  (partial_prod (Helpers.list_to_big lst) i)
;;

assert (test_partial_prod [1;2;3;4;5] 4 = 24) ;;
assert (test_partial_prod [1;2;3] 4 = 6) ;;
assert (test_partial_prod [1;2;3] 0 = 1) ;;


(* returns big_ints x, y such that ax + by = gcd(a, b) *)
let rec euclid (a: big_int) (b: big_int) : big_int * big_int =
  if eq_big_int b zero_big_int then (unit_big_int, zero_big_int)
  else let (q, r) = quomod_big_int a b in
    let (c, d) = euclid b r in (d, sub_big_int c (mult_big_int q d))
;;

(* helps test the function euclid *)
let test_euclid (a: int) (b: int) : bool =
  let (x, y) = euclid (big_int_of_int a) (big_int_of_int b) in 
    (int_of_big_int x) * a + (int_of_big_int y) * b = 1
;;

assert (test_euclid 12 25) ;;
assert (test_euclid 123 25) ;;
assert (test_euclid 1200 253) ;;

(* solves a system of modular equations *)
let solver (eqs: eq list) : big_int = 
  let res_list = List.map (fun {res; modulus} -> res) eqs in
  let mod_list = List.map (fun {res; modulus} -> modulus) eqs in
  let mod_prod = List.fold_left mult_big_int unit_big_int mod_list in
  let prod_list = List.map (fun m -> div_big_int mod_prod m) mod_list in 
  let euclid_list = List.map2 (fun m p -> euclid m p) mod_list prod_list in
  let basis_list = List.map2 (fun (c, d) p -> mult_big_int d p) euclid_list
    prod_list in
  let solution = List.map2 (fun r b -> mult_big_int r b) res_list basis_list in
  mod_big_int (List.fold_left add_big_int zero_big_int solution) mod_prod
;;

(* helps test the function solver *)
let test_solver (eqs: (int * int) list) : int  =
  let big_eqs = List.map 
    (fun (r, m) -> {res = big_int_of_int r; modulus = big_int_of_int m}) 
    eqs in
  int_of_big_int (solver big_eqs)
;;
  
assert (test_solver [(2, 3); (1, 4); (1, 5)] = 41) ;;
assert (test_solver [(2, 3); (1, 7); (1, 5)] = 71) ;;
assert (test_solver [(2, 7); (4, 11); (1, 2)] = 37) ;;

(* grabs head of a stream *)
let head (s: 'a stream) : 'a = 
  match s with 
  | Cons (h, _) -> h
;;

(* grabs tail of a stream *)
let tail (s: 'a stream) : 'a stream = 
  match s with 
  | Cons (_, t) -> Lazy.force t
;;

(* maps a function across a stream *)
let rec map (f:' a ->'b) (s: 'a stream) : 'b stream = 
  Cons (f (head s), lazy (map f (tail s)))
;;

(* the natural numbers *)
let rec nats = Cons (unit_big_int, lazy (map (add_big_int unit_big_int) nats))
;;

(* grabs the nth element of a stream *)
let rec nth (n:int) (s:'a stream) : 'a =
  let Cons (h, t) = s in if n > 0 then nth (n - 1) (Lazy.force t) else h
;;

(* filters a stream with a predicate *)
let rec filter p s = 
  if p (head s) then Cons (head s, lazy (filter p (tail s)))
  else filter p (tail s)
;;

(* The sieve of Erastosthenes, which gives a list of primes *)
let rec sieve s = 
  Cons ((head s), lazy (sieve (filter 
    ((fun a b -> not (eq_big_int (mod_big_int b a) zero_big_int)) (head s)) 
    (tail s))))
;;

(* grabs the nth prime number *)
let nth_prime n = nth (n - 1) (sieve (tail nats))
;; 

assert (int_of_big_int (nth_prime 1) = 2) ;;
assert (int_of_big_int (nth_prime 2) = 3) ;;
assert (int_of_big_int (nth_prime 3) = 5) ;;
assert (int_of_big_int (nth_prime 25) = 97) ;;
assert (int_of_big_int (nth_prime 100) = 541) ;;

(* secrets are first divided into smaller pieces, then the piece
 * splitter splits these pieces into piece-shares. a critical number
 * of piece-shares must be obtained to recover each piece. all pieces
 * must be recovered to obtain the full secret. finally each share holder
 * holds a list of piece-shares, which constitute a single share *)

(* splits pieces into piece-shares *)
let piece_splitter (moduli: big_int list) (piece: big_int) : eq list =
  List.map (fun m -> {res = mod_big_int piece m; modulus =  m}) moduli
;;
 
(* helps test the function piece_splitter *)
let test_piece_splitter (moduli: int list) (piece: int) =
  let shares = piece_splitter (List.map big_int_of_int moduli) 
    (big_int_of_int piece) in
  List.map (fun {res; modulus} -> 
    (int_of_big_int res, int_of_big_int modulus)) shares
;; 

assert (test_piece_splitter [5; 7; 11] 123 = [(3, 5); (4, 7); (2, 11)]) ;;
assert (test_piece_splitter [5; 7; 3] 1234 = [(4, 5); (2, 7); (1, 3)]) ;;
assert (test_piece_splitter [3; 7; 11] 1000000 = [(1, 3); (1, 7); (1, 11)]) ;;

(* splits secrets into pieces. the big_int list is returned with least
 * significant digit first *)
let rec secret_splitter (secret: big_int) (base: big_int) : big_int list =
  if lt_big_int secret base then [secret] 
  else let (q, r) = quomod_big_int secret base in 
    r :: secret_splitter q base
;;

(* helps test the function secret_splitter *)
let test_secret_splitter secret base =
  Helpers.list_to_int 
    (secret_splitter (big_int_of_int secret) (big_int_of_int base))
;;

assert (test_secret_splitter 1000 10 = [0;0;0;1]) ;;
assert (test_secret_splitter 10 10 = [0;1]) ;; 
assert (test_secret_splitter 10 11 = [10]) ;;
assert (test_secret_splitter 100 5 = [0;0;4]) ;;

(* combines pieces into the secret *)
let rec piece_combiner (lst : big_int list) (base: big_int) : big_int =
  match lst with
  | h :: t -> add_big_int h (mult_big_int base (piece_combiner t base))
  | [] -> zero_big_int 
;;

(* helps test the share_combiner function *)
let test_piece_combiner lst base =
  int_of_big_int 
    (piece_combiner (Helpers.list_to_big lst) (big_int_of_int base))
;;

assert (test_piece_combiner [0;0;0;1] 10 = 1000) ;;
assert (test_piece_combiner [0;1] 10 = 10) ;; 
assert (test_piece_combiner [10] 11 = 10) ;;
assert (test_piece_combiner [] 100 = 0) ;;


(* generates n moduli for use in secret sharing encryption where the
 * critical number of shares is k.
 * pass in shift = shift_index for both encryption schemes.
 * when using asmuth-bloom's scheme pass in asm = asmuth_index.
 * when using mignotte's scheme pass in asm = 0
 * in addition asmmuth_index is index of the prime that is the base in
 * secret_splitter *)

let mod_generator (n: int) (k: int) (shift: int) (asm: int) : big_int list =
  let rec mod_lister len = if len > 0 then nth_prime (len + shift) :: 
    mod_lister (len - 1) else [] in
  let mod_list = mod_lister n in
  let k_prod = partial_prod (List.rev mod_list) k in 
  let end_prod = partial_prod mod_list (k - 1) in
  let multiplier = if asm > 0 then nth_prime asm else unit_big_int in
  if gt_big_int k_prod (mult_big_int multiplier end_prod) then mod_list
  else raise (Failure "No suitable list of moduli could be generated.")
;;

assert (Helpers.list_to_int (mod_generator 20 10 100 10) =
  [659; 653; 647; 643; 641; 631; 619; 617; 613; 607; 601; 599; 593; 587; 
   577; 571; 569; 563; 557; 547]) ;;

assert (Helpers.list_to_int (mod_generator 10 10 10 0) = 
  [71; 67; 61; 59; 53; 47; 43; 41; 37; 31]) ;;

(* encode_mignotte implements the Mignotte's scheme for secret sharing by 
 * generating a list of moduli using the mod_generator, splitting the 
 * secret into pieces that are small enough to be encoded, and then 
 * splitting each of these pieces into a number of piece-shares. 
 *
 * It takes the secret as a big_int, the number of desired shares n 
 * and the number of critical shares k necessary to reconstruct 
 * the secret. It returns the shares as a list of eq lists as well as the base*)

let calc_base (n: int) (k: int) : big_int = 
  partial_prod (List.rev (mod_generator n k shift_index 0)) k
;;

let encode_mignotte (secret: big_int) (n: int) (k: int)
  : (eq list) list * big_int =
  let mod_list = mod_generator n k shift_index 0 in
  let pieces = secret_splitter secret (partial_prod (List.rev mod_list) k) in
    (List.map (piece_splitter mod_list) pieces, 
    (partial_prod (List.rev mod_list) k))
;;

(* decode_mignotte takes shares as a list of eq lists, and reconstructs the 
 * original secret by using the solver to solve the the shares representing each
 * piece and then using the piece_combiner to build the secret from these pieces
 * if the number of critical shares specified in encryption was not met, the 
 * answer returned from this function will likely be inaccurate. *)

let decode_mignotte (shares: (eq list) list) (base: big_int) : big_int =
  piece_combiner (List.map solver shares) base
;;

let rec test_mignotte (n: int) (k: int) (base: big_int) (bound: int) =
  if bound > 0 then
    (let secret = power_big_int_positive_int base bound in 
    let (shares, base) = encode_mignotte secret n k in 
    assert (eq_big_int secret (decode_mignotte shares base));
      test_mignotte n k base (bound - 1))
  else ()
;;

(* Uncomment to run test functions *)
(*
test_mignotte 8 4 (big_int_of_int 10) 50 ;;
test_mignotte 9 3 (big_int_of_int 11) 50 ;;
test_mignotte 12 6 (big_int_of_int 5) 50 ;;
test_mignotte 14 2 (big_int_of_int 12) 50 ;; 
*) 

(* Mignotte's scheme reveals some information about the secret even if the 
 * number of critical shares is not met. 
 * Asmuth-Bloom's scheme uses a random hidden
 * integer alpha, which is multiplied by a known modulus and then added
 * to each piece of the secret before they are split into shares. This alpha
 * is generated by the Random.int function using either the product of the 
 * largest (k - 1) moduli or max_int as its bound, whichever is smaller. *)

let encode_asmuth (secret: big_int) (n: int) (k: int) 
  : (eq list) list * big_int =  
  match mod_generator n k shift_index asmuth_index with
  | [] -> raise (Failure "No moduli were generated.")   
  | mod_list -> let rand_bound = min_big_int (partial_prod mod_list (k - 1))
    (big_int_of_int max_int) in 
  let alpha = big_int_of_int (Random.int (int_of_big_int rand_bound)) in
  let pieces = secret_splitter secret (nth_prime asmuth_index) in
  let asm_pieces = List.map 
    (fun x -> (add_big_int x (mult_big_int (nth_prime asmuth_index) alpha)))
    pieces in 
    (List.map (piece_splitter mod_list) asm_pieces, nth_prime asmuth_index) 
;;

(* decode_asmuth works in much the same way as decode_mignotte, except that
 * we must take the remainder of each piece divided by the modulus used 
 * during encryption in order to obtain the original information. These
 * pieces are then recombined to form the original secret. Once again, if the 
 * number of critical shares specified in encryption was not met, the answer 
 * will likely be inaccurate. *)

let decode_asmuth (shares: (eq list) list) (base: big_int) : big_int =
  (piece_combiner (List.map (fun x -> mod_big_int x base) 
    (List.map solver shares)) base)
;;

let rec test_asmuth (n: int) (k: int) (base: big_int) (bound: int) =
  if bound > 0 then
    (let secret = power_big_int_positive_int base bound in
    let (shares, base) = encode_asmuth secret n k in  
    assert (eq_big_int secret (decode_asmuth shares base));
      test_asmuth n k base (bound - 1))
  else ()
;;

let encode_string_mignotte (secret: string) (n: int) (k: int) = 
  encode_mignotte (Helpers.string_to_big_int secret) n k 
;;

let decode_string_mignotte (shares : (eq list) list) (base: big_int) =
  Helpers.big_int_to_string (decode_mignotte shares base)
;;

let encode_string_asmuth (secret: string) (n: int) (k: int) = 
  encode_asmuth (Helpers.string_to_big_int secret) n k 
;;

let decode_string_asmuth (shares : (eq list) list) (base: big_int) =
  Helpers.big_int_to_string (decode_asmuth shares base)
;;

let hacker_stream (piece_shares : eq list) : big_int stream = 
  let cong = (fun {res; modulus} x -> eq_big_int res (mod_big_int x modulus)) in
  let sols = (fun y z -> filter (cong y) z) in 
  List.fold_right sols piece_shares nats
;; 

let hack_mignotte_piece (p_s : eq list) (piece: big_int) (attempts: big_int) =
  let hack_stream = hacker_stream p_s in 
  let rec hack (h_s: big_int stream) (p: big_int) (tries: big_int) = 
    if gt_big_int tries zero_big_int then 
      eq_big_int (head h_s) p || hack (tail h_s) p 
      (sub_big_int tries unit_big_int) else false in
  hack hack_stream piece attempts
;;
      
(*
test_asmuth 8 4 (big_int_of_int 10) 50 ;;
test_asmuth 9 3 (big_int_of_int 7) 50 ;;
test_asmuth 12 4 (big_int_of_int 12) 50 ;;
test_asmuth 14 5 (big_int_of_int 12) 50 ;; 
*)

(* website dual password *)
(* research paper, compare all schemes, write hacker functions, 
 * code reviews, the writeup/video etc. verifiability, sending strings,
 * compiled code *)
