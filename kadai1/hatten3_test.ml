let add n m = fun f x ->n f (m f x);;
let mul n m = m (add n) (fun f x -> x);;
(*let sub (n: ('f->'f)->'f->'f)  (m:(('d->'j) ->'d ->'j) ) : (('e->'e) -> 'e -> 'e)=
  let next ((x:('a -> 'a)->'a ->'a),(y:('g -> 'g)->'g ->'g)) :(('h -> 'h)->'h ->'h)*(('i -> 'i)->'i ->'i) =((fun f a -> f(x f a)),x) in(*pairの第一要素の次の数とその前の数を返す関数next*)
  let pred (k :('b -> 'b)->'b ->'b ) :('c -> 'c)->'c ->'c = match (k next ((fun f x -> x), (fun f x -> x))) with(*nを受け取ってnextをn回適用させることで(n,n-1)を作り、n-1を返す関数pred*)
  | (a,b) -> b in
  m pred n ;;(*nにm回predを適用することでn-mを実装*)*)


let sub n m h z =
  let next x y s z =((s(x s z)),(x s z)) in(*pairの第一要素の次の数とその前の数を返す関数next*)
  let pred k s t= match (k next ((fun f x -> x), (fun f x -> x))) with(*nを受け取ってnextをn回適用させることで(n,n-1)を作り、n-1を返す関数pred*)
  | (a,b) -> b s t in
  (m pred n )h z;;



  let next ((x:('a -> 'b)->'a ->'b),(y:('c -> 'd)->'c ->'d)) :(('h -> 'e)->'h ->'e)*(('i -> 'j)->'i ->'j) =((fun f a -> f(x f a)),(fun f a -> x f a)) ;;
  let pred (k :('b -> 'c)->'b ->'c ) f x :'a = match (k next ((fun f x -> x), (fun f x -> x))) with(*nを受け取ってnextをn回適用させることで(n,n-1)を作り、n-1を返す関数pred*)
  | (a,b) -> b f;;
  let sub (n: ('f->'g)->'f->'g)  (m:(('d->'j) ->'d ->'j) ) (f: 'b -> 'c ) (x : 'h): 'a=(m pred n) f x;;(*nにm回predを適用することでn-mを実装*)


add (fun f x -> f (f(f x))) (fun f x -> f(f x)) (fun y -> y+1) 0;;
mul (fun f x -> f (f(f x))) (fun f x -> f(f x)) (fun y -> y+1) 0;;
(sub (fun f x -> f (f(f x))) (fun g a -> g (g a))) (fun y -> y+1) 0;;

let next (x,y) =((fun f a -> f(x f a)),x);;
let pred n f x = match n next ((fun f x -> x), (fun f x -> x)) with(*nを受け取ってnextをn回適用させることで(n,n-1)を作り、n-1を返す関数pred*)
  | (a,b) -> b f x;;
pred ( fun f x -> f (f x)) (fun y -> y+1) 0;;
match next ((fun f x -> f(f x)),(fun f x -> f(f x))) with | (a,b) -> a (fun y-> y+1) 0;;
let sub n m = 
  let mm = fun f x -> m f x in
  let nn = fun f x -> n f x in
  fun f x -> (mm pred nn) f x;;(*nにm回predを適用することでn-mを実装*)
  let h = fun g a -> g (g a) in (h pred (fun f x -> f (f (f x))) )
(fun g a -> g (g a)) pred  (fun f x -> f (f (f x)))  (fun y -> y+1) 0;;

let sub n m = m pred n;;
sub four three (fun y -> y+1) 0;;
let four = fun f x -> f(f(f(f x)));;
let three = fun f x -> f(f(f x));;
add four three (fun y -> y+1) 0;;
sub four three (fun y -> y+1) 0;;
mul four three (fun y -> y+1) 0;;


four (fun y -> y+1) 0;;
let two = fun f x -> f(f x);;
let two f x = f(f x);;
add four three (fun y ->y+1) 0;;
mul four three (fun y -> y+1) 0;;
pred (pred three) (fun y ->y+1) 0;;
pred (pred three);;
pred three;;
pred(pred three);;
two pred three;;
(two pred three) (fun y->y+1) 0;;


let subb n m =
  let toint a = a (fun y-> y+1) 0 in
  let ans = if (toint n) - (toint m) < 0 then 0 else (toint n) - (toint m) in
  let rec toch a = fun f z -> if a=0 then z 
                  else f ((toch (a-1)) f z) in
  toch ans;;

subb four two (fun y ->y+1) 0;;