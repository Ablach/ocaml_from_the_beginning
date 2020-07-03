type rect = Sq of int | Rect of int * int

let area r =
  match r with
    Sq l -> l * l
  | Rect (w,h) -> w * h

let rotate r =
  match r with
    Sq l -> Sq l
  | Rect (w,h) -> Rect (h,w)

let rotate_if_smaller r =
  match r with
    Sq l -> Sq l
  | Rect (w,h) -> if w <= h
                  then Rect (w,h)
                  else Rect (h,w)

let get_smallest r r' =
  match r, r' with
    Sq l, Sq l' -> l < l'
  | Sq l, Rect (w,h) -> l < w
  | Rect (w,h), Sq l -> w < l
  | Rect (w,h), Rect (w',h') -> w < w'

let smallest_first l = sort get_smallest (map rotate_if_smaller l)

type 'a sequence = Nil | Cons of 'a * 'a sequence

exception Seq_out_of_bounds

let rec take n s =
  if n = 0 then s else
    match s with
      Nil -> raise Seq_out_of_bounds
    | Cons (x,xs) -> Cons (x,(take (n - 1) xs))

let rec drop n s =
  if n == 0 then s else
    match s with
      Nil -> raise Seq_out_of_bounds
    | Cons (x,xs) -> drop (n - 1) s

let rec map f s =
  match s with
    Nil -> Nil
  | Cons (x,xs) -> Cons (f x, map f xs)

type expr =
  Num of int
| Add of int * int
| Sub of int * int
| Mul of int * int
| Div of int * int
| Pow of int * int

let rec eval e =
  match e with
    Num n -> n
  | Add (n,m) -> eval n + eval m
  | Sub (n,m) -> eval n - eval m
  | Mul (n,m) -> eval n * eval m
  | Div (n,m) -> eval n / eval m
  | Pow (n,m) -> power (eval n) (eval m)
