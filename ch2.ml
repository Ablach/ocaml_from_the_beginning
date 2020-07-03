let times_ten  n = n * 10

let both_non_zero n m = not (n = 0 || m = 0)

let rec n_tri n = if n = 0 then 0 else n + n_tri (n - 1)

let rec power x n = if n = 1 then x else x * power x (n - 1)

let is_consonant c = not (c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')

