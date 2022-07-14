type order = LT | EQ | GT
module type ORDERED_TYPE =
sig
  type t
  val compare : t -> t -> order
end

module type MULTISET2 =
  functor (T : ORDERED_TYPE) ->
    sig
      type t
      val empty : t
      val add    : T.t -> t -> t
      val remove : T.t -> t -> t
      val count  : T.t -> t -> int
    end

    module AbstMultiset2: MULTISET2 =(*２分木を用いた実装*)
     functor (T:ORDERED_TYPE) -> struct
       type t = Leaf | Node of T.t*t*t
       let empty = Leaf
       let rec add a t = match t with |Leaf -> Node(a,Leaf,Leaf)| Node(b,t1,t2) ->
        if T.compare a b = EQ then Node(b,Node(a,t1,Leaf),t2) else 
        if T.compare a b = LT then Node(b,(add a t1),t2) else 
        Node(b,t1,(add a t2))
      exception My_exception
      let rec getmin t = match t with | Leaf ->raise My_exception
      | Node(b,Leaf,t2) -> b
      | Node(b,t1,t2) -> getmin t1
      let rec remove a t= match t with | Leaf -> Leaf | Node(b,t1,t2) ->
        if T.compare a b = EQ then (if t2=Leaf then t1 else Node(getmin(t2),t1,(remove (getmin(t2)) t)) ) else 
        if T.compare a b = LT then Node(b,(remove a t1),t2) else 
        Node(b,t1,(remove a t2))
      let rec count a t = match t with |Leaf -> 0 |Node(b,t1,t2) ->
       if a=b then 1 + (count a t1) + (count a t2) else (count a t1) + (count a t2)
     end

(*(*以下は動作例*)
     module OrderedInt = 
     struct 
       type t = int
       let compare a b = if a < b then LT else if a > b then GT else EQ
     end
     
module IntBtree = AbstMultiset2(OrderedInt) 
let tree = IntBtree.add 4(IntBtree.add  3 (IntBtree.add  4(IntBtree.add  2(IntBtree.add 3(IntBtree.add  1(IntBtree.empty))))))
IntBtree.count 4 tree
IntBtree.count 2 tree
IntBtree.count 4 tree
let tree1 = IntBtree.remove 4 tree
let tree2 = IntBtree.remove 4 tree1
IntBtree.count 4 tree2
*)





