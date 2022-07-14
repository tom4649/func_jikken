let curry f x y= f (x,y)
let uncurry f (x,y) = f x y
exception Exception(*オリジナルの例外*)
let f x = raise Exception; (fun y ->());;
let h f = try match f 0 with | _ -> "curried uncurried function" with Exception -> "original function";;

(*
動作確認
h f;;
h (curry (uncurry f));;
*)
