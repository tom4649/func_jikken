type 'a m = 'a list
let return x = [x]
let single l = match l with
| x::[] -> true
| _ -> false
let rec single_all l = match l with 
| x::[] -> single x
| x::ls -> single x && single_all ls
| _ -> false
let (>>=) x f = 
let y = (List.map f x) in 
if (single y) || (single_all y) then List.concat y else []

let guard b = if b then return () else []

(*(*以下動作例*)
let find1 = (*はじめのlist以外のlistが要素数1でないと返り値は[]*)
  [1;2] >>= (fun x ->
  [5;6] >>= (fun y -> 
  (guard(x +y > 4) >>= (fun _ -> return (x,y))) ))
let find2 = (*全ての要素と条件を満たせば[]ではない返り値*)
    [1;2] >>= (fun x ->
    [5] >>= (fun y -> 
(guard(x +y > 4) >>= (fun _ -> return (x,y)))  ))

let find3 =(*はじめのlist以外のlistが要素数1でないと返り値は[]*)
    [1;2] >>= (fun x ->
    [4;5] >>= (fun y -> 
(guard(y = 4) >>= (fun _ -> return (x,y)))  ))

let find4 = (*全ての要素が条件を満たさないと[]*)
    [1;2] >>= (fun x ->
    [5] >>= (fun y -> 
(guard(x = 1) >>= (fun _ -> return (x,y)))  ))


(*モナド則を満たすこと*)
let m = [1;2;3]
let rec f x = [x+1]
let g x = [x-1]

let b1 = return 1 >>= f  = f 1
let b2 = m >>= return = m
let b3 = (m >>= f) >>= g = (m >>= (fun x-> (f x >>= g)))
*)
