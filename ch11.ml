type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let elem e t =
  match t with
    Leaf         -> false
  | Node (l,x,r) -> if e = x
                    then true
                    else (elem e l) || (elem e r)

let rec flip t =
  match t with
    Leaf -> Leaf
  | Node (l,x,r) -> Node (flip r, x, flip l)

let rec cong t t' =
  match t, t' with
    Leaf, Leaf -> true
  | Node (l,_,r), Node (l',_,r') -> cong l l' && cong r r'
  | _,_ -> false

let rec insert t (k,v) =
  match t with
    Leaf -> Node (Leaf, (k,v), Leaf)
  | Node (l,(k',v'),r) ->
     if k = k'
     then Node (l, (k,v), r)
     else if k < k' then Node (insert l (k,v), (k',v'), r)
     else Node (l, (k',v'), insert r (k,v))

let rec tree_of_list d =
  match d with
    [] -> Leaf
  | x :: xs -> insert (tree_of_list xs) x

let rec list_of_tree t =
  match t with
    Leaf -> []
  | Node(l,x,r) -> list_of_tree l @ [x] @ list_of_tree r

let rec combine d d' = tree_of_list (list_of_tree d @ list_of_tree d')

