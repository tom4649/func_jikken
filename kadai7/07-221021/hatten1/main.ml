(*型がついてもよさそうだが型がつかないプログラム*)
(*1*)let _= (fun f -> (f 1,f 1.))(fun x -> x)

(*2*)
let next (x,y) =((fun f a -> f(x f a)),x)

let pred n f x = match n next ((fun f x -> x), (fun f x -> x)) with(*nを受け取ってnextをn回適用させることで(n,n-1)を作り、n-1を返す関数pred*)
  | (a,b) -> b f x
let three = fun f x -> f(f(f x))
let _ = ((fun f x -> f(f x))pred three)(fun y->y+1)0(*型がつかない*)
let _ = (pred (pred three))(fun y->y+1)0(*型がつく*)
