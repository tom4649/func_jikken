let rec fold_right f lx e=
  match lx with
    | [] -> e
    | x :: ly -> f x (fold_right f ly e);;

let rec fold_left f e lx =
  match lx with
  | [] -> e
  | x::l -> fold_left f (f e x) l ;;