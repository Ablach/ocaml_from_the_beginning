let rec evens l =
  match l with
    _ :: x :: xs -> x :: evens xs
  | _ -> l


let rec count_true l =
  match l with
    [] -> 0
  | true :: xs -> 1 + count_true xs
  | _ :: xs -> count_true xs

let rec reverse l =
  match l with
    x :: xs -> reverse xs @ [x]
  | _ -> l

let rec make_palindrome xs =
  let xs' = reverse xs in xs @ xs'

let rec is_palindrome l = l = reverse l

let rec member e l =
  match l with
    [] -> false
  | [x] -> x = e
  | x :: xs -> if x = e then true else member e xs

let rec make_set l =
  match l with
    x :: xs -> if member x xs then make_set xs else x :: make_set xs
  | _ -> l

