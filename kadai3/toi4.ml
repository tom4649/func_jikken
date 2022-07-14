type order = LT | EQ | GT
module type ORDERED_TYPE =
sig
  type t
  val compare : t -> t -> order
end

module type MAP =
  functor (T : ORDERED_TYPE) ->
    sig
      type 'a t 
      val empty : 'a t
      val add    : (T. t)*'a -> 'a t -> 'a t
      val remove : T. t -> 'a t -> 'a t
      val lookup  : T. t -> 'a t -> 'a
    end
exception My_exception
exception Not_found
    
module MakeMap : MAP = 
functor (T:ORDERED_TYPE) -> struct
type 'a t = Leaf | Node of (T.t*'a)*'a t*'a t
let empty = Leaf
let rec add (key,value) t = match t with | Leaf -> Node((key,value),Leaf,Leaf) | Node((key1,value1),t1,t2) ->
  if T.compare key key1 = EQ then Node((key,value),t1,t2) else
  if T.compare key key1 = LT then Node((key1,value1),add (key,value) t1,t2) else
  Node((key1,value1),t1,add (key,value) t2) 
let rec getmin t = match t with | Leaf ->raise My_exception
  | Node(b,Leaf,t2) -> b
  | Node(b,t1,t2) -> getmin t1
let rec remove key t = match t with | Leaf -> Leaf | Node((key1,value1),t1,t2) ->
  if T.compare key key1 = EQ then (if t2=Leaf then t1 else match (getmin t2) with  (min,mv) -> Node((min,mv),t1,(remove min t2))) else
  if T.compare key key1 = LT then Node((key1,value1),remove key t1,t2) else
    Node((key1,value1),t1,remove key t2)
let rec lookup key tree = match tree with | Leaf -> raise Not_found | Node((key1,value1),t1,t2) -> 
  if T.compare key key1 = EQ then value1 else 
  if T.compare key key1 = LT then lookup key t1 else
  lookup key t2

  end
    
  module OrderedInt = 
  struct 
    type t = int
    let compare (a:t) (b:t) = if a < b then LT else if a > b then GT else EQ
  end

module Map = MakeMap(OrderedInt)

let m1 = Map.add (6,"roku")(Map.add(2,"two")(Map.add (1,"one") (Map.add (6,"six")(Map.add(0,"zero") Map.empty))))
Map.lookup 6 m1
Map.lookup 5 m1
let m2 = Map.remove 6 m1
Map.lookup 6 m2
