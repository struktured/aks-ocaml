(* return the size of a decimal to base 2 *)
let rec base2size n size =
	if n < 2
	then (size+1)
	else
	base2size (n/2) (size+1)
	;;

(* convert dec (int) to bin (string) *)
let rec dec2bin n s=
	if n < 2
	then Printf.sprintf "1" ^ s
	else
	 match n mod 2 with
		| 0 -> dec2bin (n/2) (Printf.sprintf "0" ^ s)
		| _ -> dec2bin (n/2) (Printf.sprintf "1" ^ s)
;;

(* calculate x^n mod r *)
let rec pow_mod n x r =
    if n=0 then 1 else (x * pow_mod (n-1)(x) r) mod r ;;


(* return the n-th prime number *)
 let rec getPrimeAt n x ps =
    if(List.length ps = n) then (List.hd ps) else
    if(List.exists (fun y->(x mod y=0)) ps)
    then getPrimeAt n (x+1) ps
    else getPrimeAt n (x+1) (x::ps)
;;



let rec check limit x =
	match limit > x with
		| true -> 
			| _ -> check limit (x+1)

let rec proof n r x =
 match n > r with
	| true -> 
		let nextPrime = getPrimeat (x+1) in
		| _ -> 
	
 