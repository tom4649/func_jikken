let rec fold_right f lx e=
  match lx with
    | [] -> e
    | x :: ly -> f x (fold_right f ly e);;

let rec fold_left_sub f e lx res =
  match lx with 
    | [] -> res
    | x::ly -> fold_left_sub f e ly (f res x);;

let fold_left f e lx = fold_left_sub f e lx e;;

(*apply fold_left*)
let rec fa_left lx y =
  match lx with
  | [] -> [y]
  | x::l -> x::fa_left l y;;

let append_left la lb = fold_left fa_left la lb;;

let rec ff_left (lx,f) y =
  match lx with
  | [] -> if f y then ([y],f) else ([],f)
  | x::l -> if f x then match ff_left (l,f) y with | (ly,g) -> (x::ly,g)
            else  ff_left(l,f) y;;
let filter_left_sub f l= fold_left ff_left ([],f) l;;
let filter_left f l =
  match filter_left_sub f l with
  | (lx,g) -> lx;;

filter_left (fun x-> x>5) [1;8;1;7;2;9];;

append_left [1;2;3;4] [5;6;7;8];;

(*apply fold_right*)
let fa_right x l =x::l;;
let append_right la lb= fold_right fa_right la lb;;
let ff_right y (lx,f) =
  if f y then (y::lx,f) else (lx,f);;
let filter_right_sub f l= fold_right ff_right l ([],f);;
let filter_right f l =
  match filter_right_sub f l with
  | (lx,g) -> lx;;

  filter_right (fun x-> x>5) [1;8;1;7;2;9];;

filter_left (fun x-> x>5) [1;8;1;7;2;9];;

append_left [1;2;3;4] [5;6;7;8];;
filter_right (fun x-> x>5) [1;8;1;7;2;9];;

append_right [1;2;3;4] [5;6;7;8];;