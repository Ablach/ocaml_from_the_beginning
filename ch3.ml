let not x =
  match x with
    true -> false
  | false -> true

let n_tri n =
  match n with
    0 -> 0
  | _ -> n + n_tri (n - 1)

let power x n =
  match n with
    1 -> x
  | _ -> x * power x (n - 1)

let x = match 1 + 1 with 2 -> match 2 + 2 with 3 -> 4 | 4 -> 5

let isLower c =
  match c with
    'a'..'z' -> true
  | _ -> false

let isUpper c =
  match c with
    'A'..'Z' -> true
  | _ -> false
