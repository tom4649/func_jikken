let rec fold_right f lx e=
  match lx with
    | [] -> e
    | x :: ly -> f x (fold_right f ly e);;

    let rec fold_left f e lx =
      match lx with
      | [] -> e
      | x::l -> fold_left f (f e x) l ;;

(*apply fold_left*)
(*append_leftの実装のためにfold_leftに与える関数fa_left*)
let rec fa_left lx y =
  match lx with
  | [] -> [y]
  | x::l -> x::fa_left l y;;

let append_left la lb = fold_left fa_left la lb;;
(*filter_leftの実装のためにfold_leftに与える関数ff_left*)
let rec ff_left (lx,f) y =
  match lx with
  | [] -> if f y then ([y],f) else ([],f)
  | x::l -> if f x then match ff_left (l,f) y with | (ly,g) -> (x::ly,g)
            else  ff_left(l,f) y;;
let filter_left_sub f l= fold_left ff_left ([],f) l;;
let filter_left f l =
  match filter_left_sub f l with
  | (lx,g) -> lx;;

(*apply fold_right*)
(*append_rightの実装のためにfold_rightに与える関数fa_right*)
let fa_right x l =x::l;;
let append_right la lb= fold_right fa_right la lb;;
(*filter_rightの実装のためにfold_rightに与える関数ff_right*)
let ff_right y (lx,f) =
  if f y then (y::lx,f) else (lx,f);;
let filter_right_sub f l= fold_right ff_right l ([],f);;
let filter_right f l =
  match filter_right_sub f l with
  | (lx,g) -> lx;;
