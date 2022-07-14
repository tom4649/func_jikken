module AbstStack :
sig
type 'a t
val pop: 'a t -> ('a* 'a t)
val push : 'a -> 'a t -> 'a t
val empty : 'a t
val size : 'a t -> int
end
=
struct
type 'a t = 'a list
exception My_exception
let pop s = 
  match s with
  | [] -> raise My_exception
  | x::res -> (x,res)
let push a s = a::s
let empty = []
let rec size_sub s k = match s with
| [] -> k
| a::res -> size_sub res k+1
let size s = size_sub s 0  
end
