let phi =
  let rec euler n x ps ss =
    (* if( x mod 100 = 0) then (Printf.printf "%d\n" x) else (); *)
    if (x>n) then n else
    if (List.exists (fun y -> (n mod x=0) ) ps)
    then x::ss
				 euler (n / x) (x) ps ss
    else euler n (x+1) ps ss
  in
    euler 14 0 [] []
;;

