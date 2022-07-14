type 'a tree =
| Leaf
| Node of 'a * 'a tree * 'a tree;;

let level_order' tree =
  let rec level_order_sub que ans =
    match que with
    | [] -> ans
    | tree::res -> match tree with
      | Leaf -> level_order_sub res ans
      | Node(a,b,c) -> if res = [] then level_order_sub [b;c] (ans@[a]) 
      else level_order_sub (res@[b]@[c]) (ans@[a])
    in level_order_sub ([tree]) [];;

  


