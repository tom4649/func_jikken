let twice f = fun x->  f (f x) ;;

let rec repeat_sub f y n =
  if n=0 then y
  else repeat_sub f (fun x-> f(y x)) (n-1);;

let repeat f n =repeat_sub f f (n-1);;

repeat (fun x -> 2*x) 4 3;;
