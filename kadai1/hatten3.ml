(*church encoding　本来の実装*)
let add n m = fun f x ->n f (m f x);;
let mul n m = m (add n) (fun f x -> x);;
(*subはchurch encodingを整数に直して計算したあと再びchurch encodingに直すことによって実装*)
let sub n m =
  let ch_to_int a = a (fun y-> y+1) 0 in(*chrch encodingを整数に直す関数ch_to_int*)
  let ans = if (ch_to_int n) - (ch_to_int m) < 0 then 0 else (ch_to_int n) - (ch_to_int m) in(*整数に直して求める値を計算*)
  let rec to_ch a = fun f z -> if a=0 then z 
                  else f ((to_ch (a-1)) f z) in(*整数をchurch数に直す関数to_ch*)
  to_ch ans;;

  (*レコードを使った実装*)
type myint = {var : 'a. ('a->'a) -> 'a -> 'a};;
let sub' n m h z =
  let next (x,y) =({var = fun f a -> (f(x.var f a))},{var = fun f a -> (x.var f a)}) in(*pairの第一要素の次の数とその前の数を返す関数next*)
  let pred k = match (k.var next ({var = (fun f x -> x)}, {var = (fun f x -> x)})) with(*nを受け取ってnextをn回適用させることで(n,n-1)を作り、n-1を返す関数pred*)
  | (a,b) -> b in
   ((m.var pred n)).var h z;;

(*以下は使用例*)
   let four' = {var = fun f x -> f(f(f(f x)))};;
   let three' = {var = fun f x -> f(f(f x))};;

   sub' four' three' (fun y -> y+1) 0;;

(*以下はエラーを解消できなかったコード
let sub n m h z =
  let next x y s z =((s(x s z)),(x s z)) in(*pairの第一要素の次の数とその前の数を返す関数next*)
  let pred k s t= match (k next ((fun f x -> x), (fun f x -> x))) with(*nを受け取ってnextをn回適用させることで(n,n-1)を作り、n-1を返す関数pred*)
  | (a,b) -> b s t in
  (m pred n )h z;;

pred(pred three)(fun y->y+1)0;;とすると1が正しく得られるが、
((fun f x -> f (f x)) pred three)(fun y->y+1)0;;とすると型が合わずにエラーが生じた。
*)
