let rec fold_right f lx e=
  match lx with
    | [] -> e
    | x :: ly -> f x (fold_right f ly e);;

let rec fold_left_sub f e lx res =
  match lx with 
    | [] -> res
    | x::ly -> fold_left_sub f e ly (f res x);;

let fold_left f e lx = fold_left_sub f e lx e;;

fold_left (fun x y -> x*x*y*y) 1 [1;2;3];;
fold_right (fun x y -> x*x*y*y) [1;2;3] 1;;

