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