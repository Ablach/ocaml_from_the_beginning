let rec map f l =
  match l with
    [] -> []
  | h :: t -> f h :: map f t

let rec calm l =
  match l with
    [] -> []
  | '!' :: cs -> '.' :: cs
  | c :: cs -> c :: calm cs

let calm' l = map (fun c -> if c == '!' then '.' else c) l

let clip n = if n < 1 then 1 else if n > 10 then 10 else n

let clip_list l = map clip l

let clip_list' l = map (fun n ->
                         if n < 1 then 1
                         else if n > 10 then 10 else n) l

let rec apply f n a =
  match n with
    0 -> a
  | _ -> apply f (n - 1) (f a)

let rec insert cmp x l =
  match l with
    [] -> [x]
  | h :: t ->
     if cmp x h
     then x :: h :: t
     else h :: insert cmp x t

let rec sort cmp l =
  match l with
    [] -> []
  | h :: t -> insert cmp h (sort cmp t)

let rec filter p l =
  match l with
    [] -> []
  | x :: xs -> if p x
               then x :: filter p xs
               else filter p xs

let not b = if b then false else true

let for_all p l =
  match filter (fun x -> not (p x)) l with
    [] -> true
  | _ -> false

let mapl f l = map (fun l' -> f l') l

