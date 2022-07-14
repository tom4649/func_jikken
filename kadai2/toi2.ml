type 'a tree =
| Leaf
| Node of 'a * 'a tree * 'a tree;;

(*行きがけ順*)
let rec pre_order tree =
  match tree with
  | Leaf -> []
  | Node(a,b,c) -> a::(pre_order b )@ (pre_order c);;

(*通りがけ順*)  
let rec in_order tree =
  match tree with
  | Leaf -> []
  | Node(a,b,c) ->
    (in_order b)@ [a]@(in_order c);;
(*帰りがけ順*)
let rec post_order tree =
  match tree with
  | Leaf -> []
  | Node(a,b,c) -> (post_order b)@(post_order c)@[a];;  

let tree = Node(6,Node(7,Node(3,Node(8,Leaf,Leaf),Leaf),Node(4,Node(2,Leaf,Leaf),Leaf)),Node(1,Node(6,Leaf,Leaf),Node(5,Leaf,Leaf)));;
pre_order tree;;
in_order tree;;
post_order tree;;