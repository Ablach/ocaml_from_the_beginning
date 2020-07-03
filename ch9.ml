let member_all e ls =
  match
    filter ((!=) true) (map (member e) ls)
  with
    [] -> true
  | _ -> false

let flip_div n m = m / n

let mapll f = map (map f)

let truncate n = map (fun l -> try take n l with
                                 Match_failure _ -> l)

let first d l =
  match l with
    [] -> d
  | (x :: _) -> x

let firsts d = map (first d)
