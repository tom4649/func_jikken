type 'a result = Error of string | Ok of 'a

module IntIntStorage =(*メモを表すモジュール*)
struct 
type t = (int*int)list
let empty = []
let add a xs = a::xs
let  rec eLookup  x l = match l with |[] -> Error("Not found")
                                      |  (a,b)::res -> if x = a then Ok(b) else eLookup x res
end

type 'a m = IntIntStorage.t -> 'a*IntIntStorage.t
let (>>=) (x:'a m) (f:'a->'b m) = (fun s->  let (a,s') = x s in (f a) s':'b m)
let return a = (fun s -> (a,s) :'a m)
let memo (f:int->int m) (n:int) = (fun s ->
  match IntIntStorage.eLookup n s with
  | Ok(b)-> (b,s)
  | Error(_) ->  let (b,s') = (f n) s in
    (b,IntIntStorage.add (n,b) s') :int m)
let runMemo (m:'a m) = let (ans,_) = m [] in ans


(*(*動作例*)
(*スライドの動作例*)
let rec fib n =
  if n <= 1 then return n
  else 
    (memo fib (n-1)) >>= (fun r1 ->
    (memo fib (n-2)) >>= (fun r2 ->
      return (r1+r2)));;
runMemo (fib 80);;
(*階乗の計算*)
let rec fact n =
  if n <= 1 then return n
  else 
    (memo fact (n-1)) >>= (fun r1 ->
      return (r1*n));;

runMemo(fact 10);;
(*モナド則を満たすことの確認*)
runMemo ((return 3) >>= fib) = runMemo (fib 3);;
let y = (fun s -> (3,s));;
( y >>= return ) IntIntStorage.empty = y IntIntStorage.empty;;
runMemo((y>>=fib) >>= fact) = runMemo(y>>=(fun x -> fib x >>= fact));;
*)
