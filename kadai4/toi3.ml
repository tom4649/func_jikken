type 'a m = 'a*string
let (>>=) x f = match x with 
| (a,s) ->( match f a with (b,t)-> (b,s^t))
let return a = (a,"")
let writer s = ((),s)
let f x =
  (x+1,"call f("^(string_of_int x)^"), ")
  let g x =
    (2*x,"call g("^(string_of_int x)^"), ");;
(f 3) >>= (fun a ->
  (g a)>>= (fun b->
    (f b) >>= (fun c-> return c)));;

(return 1) >>= f ;;
(1,"hello") >>= return;;
((1,"hello ") >>= f) >>= g;; 
(1,"hello ") >>= (fun x -> f x >>= g);;
