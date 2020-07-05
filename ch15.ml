let rec concat xs ys =
  match xs,ys with
    [], ys -> ys
  | x::xs', ys -> x :: concat xs' ys

let all_true ls = map (List.mem true) ls

let exclamations s =
  let n = ref 0 in
  String.iter
    (fun c -> if c = '!' then n := !n + 1)
    s;
  n

let calm s =
  String.map
    (fun c -> if c = '!' then '.' else c)
    s

let concat_strs ss = String.concat "" ss

let concat_bfrs bs = let b = List.hd bs in
                     for i = 1 to List.length bs - 1 do
                       Buffer.add_buffer b (List.nth bs i)
                     done;
                       b

let get_next s i i' =
  try
    String.sub s i i'
  with
    Invalid_argument _ -> ""

let rec ocaml_sub s =
  if s = "" || String.length s < String.length "Ocaml" then 0 else
    try 
      let i = String.index s 'O' in
      let occ = get_next s i (5) in
      if occ = "Ocaml"
      then 1 + ocaml_sub (get_next s (i + 5) (String.length s - (i + 5)))
      else ocaml_sub (get_next s (i + 1) (String.length s - 1))
    with
      Not_found -> 0
