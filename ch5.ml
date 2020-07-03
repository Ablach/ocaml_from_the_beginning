let rec length l =
  match l with
    [] -> 0
  | h::t -> 1 + length t

let rec take n l =
  if n = 0 then [] else
    match l with
      h::t -> h::take (n - 1) t

let rec drop n l =
  if n = 0 then l else
    match l with
      h::t -> drop (n - 1) t

let rec merge xs ys =
  match xs, ys with
    [], l -> l
  | l, [] -> l
  | x :: xs', y :: ys' ->
     if x < y
       then x :: merge xs' (y :: ys')
       else y :: merge (x :: xs') ys'

let rec msort l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
     let len_over_2 = length l / 2 in
     let left = take len_over_2 l in
     let right = drop len_over_2 l in
     merge (msort left) (msort right)

let rec insert x l =
  match l with
    [] -> [x]
  | h :: t ->
     if x <= h
     then x :: h :: t
     else h :: insert x t

let rec insert_rev x l =
  match l with
    [] -> [x]
  | h :: t ->
     if x <= h
     then h :: insert_rev x t
     else x :: h :: t

let rec sort l =
  match l with
    [] -> []
  | h :: t -> insert h (sort t)

let rec rev_sort l =
  match l with
    [] -> []
  | h :: t -> insert_rev h (sort t)

let is_sorted l = l = sort l

let rec insertion_sort l =
  let rec ins x l' =
    match l' with
      [] -> [x]
    | h' :: t' ->
       if x <= h'
       then x :: h' :: t'
       else h' :: ins x t'
  in match l with
       [] -> []
     | h :: t -> ins h (insertion_sort t)
