let rec keys dict =
  match dict with
    [] -> 0
  | (_,_) :: xs -> 1 + keys xs

let replace k v d =
  match d with
    [] -> raise Not_found
  | (k, _) :: xs -> (k,v) :: xs

let rec zip keys vals =
  match keys, vals with
    [], [] -> []
  | k, [] -> raise (Invalid_argument "zip")
  | [], v -> raise (Invalid_argument "zip")
  | k :: ks, v :: vs ->
     (k,v) :: zip ks vs

let rec foldr f a l =
  match l with
    [] -> a
  | (x :: xs) -> f x (foldr f a xs)

let rec unzip dict = foldr
                       (fun (k,v) (kl, vl) -> (k :: kl, v :: vl))
                       ([],[]) dict

let rec to_dict l =
  match l with
    [] -> []
  | (k,v) :: xs -> match filter
                           (fun (a, _) -> not ((=) k a)) xs
                   with
                     [] -> [(k,v)]
                   | d -> (k,v) :: to_dict d
