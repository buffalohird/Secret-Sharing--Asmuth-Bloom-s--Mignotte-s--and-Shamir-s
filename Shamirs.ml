open Nat ;;
open Big_int ;;
type poly = big_int list ;;
type point = big_int * big_int ;;

(***** Shamir's Encode Functions *****)

(* Generates a random polynomial by accepting the constant term 
 * and the length of the polynomial *)
let rand_poly (secret: big_int) (critical_shares: int) : poly =
    let needed_coefficients = critical_shares - 1 in 
    let rec poly_builder s n = 
      if n < 0 then failwith "Need a positive number of critical shares"
      else if n > 0 then poly_builder 
	(s @ [big_int_of_int (Random.int max_int)]) (n - 1)
      else s 
    in poly_builder [secret] needed_coefficients
;;

(* Tests if rand_poly generated a properly lengthed polynomial *)
let rand_poly_size_tester (polynomial: poly) =
  List.length polynomial
;;

let big123 = big_int_of_int 123 ;;

assert (rand_poly_size_tester (rand_poly big123 1) = 1) ;;
assert (rand_poly_size_tester (rand_poly big123 5) = 5) ;;
assert (rand_poly_size_tester (rand_poly big123 100) = 100) ;;

(* Test if rand_poly generated a polynomial with correct constant term *)
let rand_poly_secret_tester (polynomial: poly) = 
  match polynomial with
  | h :: t -> h
  | _ -> failwith "Empty polynomial"
;;

assert (int_of_big_int (rand_poly_secret_tester (rand_poly big123 5)) 
  = int_of_big_int (big123)) ;;

(* Returns p(x) for a given p and x *)
let poly_output (p: poly) (x: int) : big_int =
  let rev_poly = List.rev p in
  let rec rev_poly_output p x =
    match p with
    | [] -> failwith "Empty polynomial"
    | h :: [] -> h
    | h :: t -> let degree = (List.length p) - 1 in
        add_big_int (mult_big_int (power_big_int_positive_int 
        (big_int_of_int x) degree) h) (rev_poly_output t x)
  in rev_poly_output rev_poly x
;;

assert (int_of_big_int (poly_output (Helpers.list_to_big [1;2;3]) 3) = 34) ;;
assert (int_of_big_int (poly_output (Helpers.list_to_big [1;2]) 3) = 7) ;;
assert (int_of_big_int (poly_output (Helpers.list_to_big [1]) 3) = 1) ;;
		
(* encodes a secret value using Shamir's Scheme; returns a list of points 
 * to be used in Lagrange interpolation *)
let encode_shamirs (secret: big_int) (total_shares: int) (critical_shares: int)
  : point list =
  if critical_shares > total_shares then failwith "Must follow: K =< N"
  else 
    let r_poly = rand_poly secret critical_shares in
    let rec point_generator s k n list = 
      if n = 0 then list
      else point_generator s k (n - 1) 
        [(big_int_of_int n, poly_output r_poly n)] @ list
    in 
    point_generator secret critical_shares total_shares []
;;


(* Note: Shamir's encoding is difficult to test as polynomials are generated 
 * randomly, and the points that comprise the shares
 * often have y values that exceed max_int 
 * We will effectively test this function in decode_shamirs *)

(***** Helpers for testing and higher-order functions  *****)

(* typical reduce function *)
let rec reduce (f: 'a -> 'b -> 'b) (u: 'b) (xs: 'a list) : 'b =
  match xs with
  | [] -> u
  | h :: t -> f h (reduce f u t)
;;

(* tests if two polys are equal *)
let rec eq_poly (p1: poly) (p2: poly) : bool =
  match (p1, p2) with
  | [], [] -> true
  | h1 :: t1, h2 :: t2 -> eq_big_int h1 h2 && eq_poly t1 t2
  | _, _ -> false
;;

assert (eq_poly [] []) ;;
assert (eq_poly (Helpers.list_to_big [1;2;3]) 
                (Helpers.list_to_big [1;2;3])) ;;
assert (not (eq_poly (Helpers.list_to_big [1;2;3]) 
                     (Helpers.list_to_big [1;2]))) ;;

(* grabs the x coordinate from a point *)
let get_point_x (p : point) : big_int = 
  match p with
  | (x, y) -> x
;;

(* grabs the y coordinate from a point *)
let get_point_y (p : point) : big_int =
  match p with
  | (x, y) -> y
;;

(* trims down a list to length k *)
let rec list_cutter k lst =
  if k > 0 then
    match lst with
    | h :: t -> h :: list_cutter (k - 1) t
    | [] -> []
  else []
;;

assert (list_cutter 3 [1;2;3;4;5;6] = [1;2;3]) ;;
assert (list_cutter 0 [1;2;3;4;5;6] = []) ;;
assert (list_cutter 3 [] = []) ;;
assert (list_cutter 4 [1;2;3] = [1;2;3]) ;;

(***** Shamir's Decode Functions *****)

(* multiplies the polynomial p by the binomial (x + a0) *)
let mult_binom (a0: big_int) (p: poly) =
  let a = [zero_big_int] @ p in
  let b = List.map (fun c -> mult_big_int c a0) p @ [zero_big_int] in
  List.map2 add_big_int a b
;;

assert (eq_poly (mult_binom (big_int_of_int 3) (Helpers.list_to_big [1;2;3]))
  (Helpers.list_to_big [3;7;11;3])) ;;

(* generates the polynomial (x + x1) * (x + x2) * ... * (x + xn) *)
let generate_polynomial (xs: big_int list) : poly =
  reduce mult_binom [unit_big_int] xs
;;  

assert (eq_poly (generate_polynomial (Helpers.list_to_big [1;1;1;1])) 
  (Helpers.list_to_big [1;4;6;4;1])) ;;

(* computes (x0 - x1) * (x0 - x2) * ... * (x0 - xn) *)
let generate_denom (x0: big_int) (xs: big_int list) : big_int =
  let new_xs = List.map (fun x -> sub_big_int x0 x) xs in
  reduce (fun x y -> mult_big_int x y) unit_big_int new_xs
;;

assert (eq_big_int (generate_denom (big_int_of_int 3) 
  (Helpers.list_to_big [1;2;4;5])) (big_int_of_int 4)) ;;

(* generates a lagrange basis polynomial for each point x0 
 * returns a tuple of the numerator and denominator *)
let lagrange_basis_calc (x0: big_int) (lst: big_int list) : poly * big_int =
  let others_list = List.filter (fun x -> not (eq_big_int x x0)) lst in
  let neg_others_list =  List.map (fun x -> minus_big_int x) others_list in 
  (generate_polynomial neg_others_list, generate_denom x0 others_list)
;;

let (lst, big) = lagrange_basis_calc (big_int_of_int 3) 
  (Helpers.list_to_big [1;2;3]) in
  assert (eq_poly lst (Helpers.list_to_big [2;-3;1]));
  assert (int_of_big_int big = 2) ;;

(* generates all lagrange basis polynomials *)
let lagrange_basis (p_list : point list) : (poly * big_int) list =
  let x_list = List.map (fun x -> get_point_x x) p_list in
  List.map (fun x -> lagrange_basis_calc x x_list) x_list
;;

match lagrange_basis (List.map
  (fun (x, y) -> (big_int_of_int x, big_int_of_int y)) [(1,1);(2,2);(3,3)]) 
with
| [(l1, b1); (l2, b2); (l3, b3)] ->  
  assert (eq_poly l1 (Helpers.list_to_big [6;-5;1]));
  assert (int_of_big_int b1 = 2);
  assert (eq_poly l2 (Helpers.list_to_big [3;-4;1]));
  assert (int_of_big_int b2 = -1);
  assert (eq_poly l3 (Helpers.list_to_big [2;-3;1]));
  assert (int_of_big_int b3 = 2)
| _ -> failwith "failed test"
;;

(* multiplies a polynomial by a scalar *)
let multiply_poly_by_scalar (p : poly) (s: big_int) : poly =
  List.map (fun x -> mult_big_int x s) p
;;

assert (eq_poly (multiply_poly_by_scalar (Helpers.list_to_big [1;2;3])
  (big_int_of_int 4)) (Helpers.list_to_big [4;8;12])) ;;

(* divides a polynomial by a scalar *)
(* WARNING: uses integer division *)
let divide_poly_by_scalar (p : poly) (s: big_int) : poly =
  List.map (fun x -> div_big_int x s) p
;;

assert (eq_poly (divide_poly_by_scalar (Helpers.list_to_big [4;8;12])
  (big_int_of_int 4)) (Helpers.list_to_big [1;2;3])) ;;

(* creates a polynomial sum by multiplying each poly by its respective 
 * coefficient, which is determined by the original points passed into 
 * decode_shamirs *)
let rec scale_by_ys (frac_polys : (poly * big_int) list) (pl: point list) 
  : (poly * big_int) list =
  match (frac_polys, pl) with
  | ([], []) -> []
  | ((n, d) :: t_frac, h :: t_point) -> let y = get_point_y h in
      (multiply_poly_by_scalar n y, d) :: (scale_by_ys  t_frac t_point)
  | (_, _) -> failwith "poly list and point list lengths not equal"
;;

assert (scale_by_ys [] [] = []) ;;

let pts = (List.map (fun (x, y) -> (big_int_of_int x, big_int_of_int y)) 
  [(1,1);(2,2);(3,3)]) in 
match scale_by_ys (lagrange_basis pts) pts with
| [(l1, b1); (l2, b2); (l3, b3)] ->
  assert (eq_poly l1 (Helpers.list_to_big [6;-5;1]));
  assert (int_of_big_int b1 = 2);
  assert (eq_poly l2 (Helpers.list_to_big [6;-8;2]));
  assert (int_of_big_int b2 = -1);
  assert (eq_poly l3 (Helpers.list_to_big [6;-9;3]));
  assert (int_of_big_int b3 = 2)
| _ -> failwith "failed test"
;;

(* sums polynomials that are in fractional form *)
(* each poly must have the same number of coefficients *)
let rec sum_frac_polys (frac_polys : (poly * big_int) list) : poly * big_int =
  let base_case = 
    let (base_numerator, _) = List.hd frac_polys in
    List.map (fun x -> zero_big_int) base_numerator in
  let sum_frac (p1, d1) (p2, d2) : poly * big_int =
    (List.map2 add_big_int (multiply_poly_by_scalar p1 d2) 
      (multiply_poly_by_scalar p2 d1), mult_big_int d1 d2) in
  reduce (fun x y -> sum_frac x y) (base_case, unit_big_int) frac_polys
;;

let pts = (List.map (fun (x, y) -> (big_int_of_int x, big_int_of_int y)) 
  [(1,1);(2,2);(3,3)]) in
let (lst, big) = sum_frac_polys (lagrange_basis pts) in
  assert (eq_poly lst (Helpers.list_to_big [-4;0;0]));
  assert (int_of_big_int big = -4)
;;

(* returns the secret from the final secret poly produced by decode *)
let extract_secret (p : poly) : big_int =
  match p with
  | [] -> failwith "no coefficients in poly"
  | h :: _ -> h
;;

assert (int_of_big_int (extract_secret (Helpers.list_to_big [1;2;3])) = 1) ;;

(* divides the denominator of a polynomial into the numerator *)
let poly_of_frac_poly (frac_poly: poly * big_int) =
  let (p, d) = frac_poly in divide_poly_by_scalar p d
;;   

assert (eq_poly (poly_of_frac_poly (Helpers.list_to_big [3;12;21], 
  big_int_of_int 3)) (Helpers.list_to_big [1;4;7])) ;;
  
(* decodes a list of points into the secret *)
let decode_shamirs (pl : point list) (k: int) : big_int =
  let new_pl = list_cutter k pl in
  extract_secret (poly_of_frac_poly (sum_frac_polys (scale_by_ys 
    (lagrange_basis new_pl) new_pl)))
;;

(* helps test shamir's scheme of encryption and decryption
 * n is the total number of shares
 * k is the critical number of shares 
 * secrets range from base to base ^ bound *)
let rec test_shamirs (n: int) (k: int) (base: big_int) (bound: int) =
  if bound > 0 then
    (let secret = power_big_int_positive_int base bound in 
    let points = encode_shamirs secret n k in 
    assert (eq_big_int secret (decode_shamirs points k));
      test_shamirs n k base (bound - 1))
  else ()
;;

test_shamirs 10 5 (big_int_of_int 7) 1000 ;;
test_shamirs 11 6 (big_int_of_int 8) 1000 ;;
test_shamirs 4 2 (big_int_of_int 3) 1000 ;;
test_shamirs 2 1 (big_int_of_int 20) 1000 ;;
