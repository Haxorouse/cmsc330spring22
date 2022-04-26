(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
    match tup with 
    (a, b, c) -> (c, b, a);;

let is_odd x =
    if (x mod 2) = 0 then false else true;;

let area x y = 
match x with (a, b) ->
match y with (c, d) ->
    abs((a-(c)) * (b-(d)));;

let volume x y = 
match x with (a, b, c) ->
match y with (d, e, f) ->
    abs((a-(d)) * (b-(e)) * (c-(f)));;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n =
    match n with
    0 -> 0
    | 1 -> 1
    | _ ->
    (fibonacci (n-1)) + (fibonacci (n-2));;

let rec pow x y =
    match y with
    0 -> 1
    | _ ->
    x * (pow x (y-1));;

let rec log x y = 
    if(y/x) < 1 then 0
    else 1 + (log x (y/x));;

let rec gcf x y = 
    if x = 0 then y
    else gcf (y mod x) x;;

let rec is_prime_recurse x d =
    if (x/d) < d then true
    else if (x mod d) = 0 then false
    else is_prime_recurse x (d+1);;

let rec is_prime x =
    match x with
    0 -> false
    | 1 -> false
    | _ -> if x < 0 then false
    else is_prime_recurse x 2;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = failwith "unimplemented"

let larger lst1 lst2 = failwith "unimplemented"

let reverse lst = failwith "unimplemented"

let rec combine lst1 lst2 = failwith "unimplemented"

let rec merge lst1 lst2 = failwith "unimplemented"

let rec rotate shift lst = failwith "unimplemented"

let rec is_palindrome lst = failwith "unimplemented"