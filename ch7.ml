let smallest l =
  match filter (fun x -> x >= 0) l with
    [] -> raise Not_found
   |(x :: xs) -> let rec smallest' x' xs' =
                   match xs' with
                     [] -> x'
                   | (h :: t) -> if x' < h
                                 then smallest' x' t
                                 else smallest' h t
                 in smallest' x xs

let smallest_or_zero l = try smallest l with
                           Not_found -> 0

exception Neg of int

let floor_sqrt n = if n < 0 then raise (Neg n)
                   else let rec floor_sqrt' i n =
                          if i * i > n
                          then i - 1
                          else floor_sqrt' (i + 1) n
                        in floor_sqrt' 1 n

let floor_sqrt_or_zero n = try floor_sqrt n with
                             Neg n -> 0

