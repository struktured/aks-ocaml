(*  only insert value of x into lst, if unique *)
let unique x lst = if List.exists (fun y -> x = y) lst then lst else x :: lst
  
(* return the list with the factors of n *)
let rec factors n x lst =
  if x > n
  then lst
  else
    (match n mod x with
     | 0 -> factors (n / x) x (unique x lst)
     | _ -> factors n (x + 1) lst)
  
(* calculate the numbers of integers coprime to s *)
let rec euler lst s =
  match lst with
  | [] -> s
  | head :: tail -> euler tail (s *. (1. -. (1. /. (float_of_int head))))
  
(* the "main" function of the euler-totient-function *)
let euler_totient n =
  let factorLst = factors n 2 []
  in
    (* List.iter (fun v -> Printf.printf "%d::" v) factorLst; *)
    int_of_float ((float_of_int n) *. (euler factorLst 1.))
  
