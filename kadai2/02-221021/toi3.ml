type 'a tree =
| Leaf
| Node of 'a * 'a tree * 'a tree;;

(*listをqueueとして用いることで幅優先探索を実装*)
let level_order tree =
  let rec level_order_sub que ans =
    match que with
    | [] -> ans
    | tree::res -> match tree with
      | Leaf -> level_order_sub res ans
      | Node(a,b,c) -> if res = [] then level_order_sub [b;c] (ans@[a]) 
      else level_order_sub (res@[b]@[c]) (ans@[a])
    in level_order_sub ([tree]) [];;

(*listの実装*)
type 'a mlist =
| QNil
| QCons of 'a*'a mlist ref;;

(*listをqueueとして用いた*)
let rec push a q =
  match q with
  |  QCons(_,id)-> if !id = QNil then let nid=ref QNil in id := QCons(a,nid) else push a !id
  | QNil -> ();;
(*queueを用いて幅優先探索を実装*)
let level_order' tree =
  let rec level_order_sub que ans =
    match que with
    | QNil -> ans
    | QCons(tree,id) -> match tree with
      | Leaf -> level_order_sub (!id) ans
      | Node(a,b,c) -> if !id = QNil then let nid = ref (QCons(c,ref QNil)) in (level_order_sub (QCons(b, nid)) (ans@[a]) )
      else (push b (!id);push c (!id);level_order_sub (!id) (ans@[a]))
    in level_order_sub (QCons(tree,ref QNil)) [];;
