let rec fold_right f lx e=
  match lx with
    | [] -> e
    | x :: ly -> f x (fold_right f ly e);;

let rec fold_left f e lx =
  match lx with
  | [] -> e
  | x::l -> fold_left f (f e x) l ;;

let fold_left_by_right f e l=
  let f_by_right x k= fun acc -> k(f acc x) in
  (fold_right f_by_right l (fun a->a)) e;;

let fold_right_by_left f l e =
  let f_by_left k x= fun acc ->  k(f x acc) in
  (fold_left f_by_left (fun a-> a) l) e;;
  