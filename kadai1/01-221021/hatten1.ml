(*末尾再帰でリストを逆順にする関数reverse_subを定義*)
let rec reverse_sub la res =
  match la with
  | [] -> res
  | a::l -> reverse_sub l (a::res) ;;
let reverse l = reverse_sub l [];;

(*fold_rightを用いてreverse_rightを実装*)
let rec fold_right f lx e=
  match lx with
    | [] -> e
    | x :: ly -> f x (fold_right f ly e);;
(*関数合成を利用した再帰関数によってfold_rightに渡す関数を定義*)
let rec reverse_f x g = fun l -> g (x::l);;

let reverse_right l = (fold_right reverse_f l (fun lx -> lx) )[];; 
