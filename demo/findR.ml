(* return the size of a decimal to base 2 *)
let rec base2size n size =
  match n < 2 with 
		| true -> size + 1 
		| _ -> base2size (n / 2) (size + 1)
  
(* convert dec (int) to bin (string) *)
let rec dec2bin n s =
  if n < 2
  then (Printf.sprintf "1") ^ s
  else
    (match n mod 2 with
     | 0 -> dec2bin (n / 2) ((Printf.sprintf "0") ^ s)
     | _ -> dec2bin (n / 2) ((Printf.sprintf "1") ^ s))
  
(* calculate x^n mod r *)
let rec pow_mod n x r =
  match n with 
		| 0 -> 1
		| _ -> (x * (pow_mod (n - 1) x r)) mod r
  
(* return the n-th prime number *)
let rec getPrimeAt n x ps =
  if (List.length ps) = n
  then List.hd ps
  else
    if List.exists (fun y -> (x mod y) = 0) ps
    then getPrimeAt n (x + 1) ps
    else getPrimeAt n (x + 1) (x :: ps)


let rec innerloop limit n x r =
	let failed = ((pow_mod n x r) = 1) in
	match limit > x with
	| false -> failed
	| _ ->  match failed with
					| true -> failed
					| _ -> innerloop limit n (x+1) r
;;
		  


let rec findR n p =
	let limit = ((base2size n 0) * (base2size n 0))  in
	let r = getPrimeAt p 2 [] in
	let found = innerloop limit n 1 r in 
	match r > n with
    | true -> findR n (p+1)
		| _ -> found
;;
							


	
