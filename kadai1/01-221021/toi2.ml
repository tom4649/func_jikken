let twice f = fun x->  f (f x) ;;

(*関数yにn回関数fを合成した関数を返す関数repeat_subを定義*)
let rec repeat_sub f y n =
  if n=0 then y
  else repeat_sub f (fun x-> f(y x)) (n-1);;

let repeat f n =repeat_sub f f (n-1);;
