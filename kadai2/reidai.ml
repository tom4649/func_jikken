type complex = {re : float; im : float} ;;
let prod a b = {re = a.re *.b.re -. a.im *.b.im ; im = a.re*.b.im +.a.im*.b.re};;

type str_tree =
| Leaf
| Node of string*str_tree*str_tree;;

let tree1 = Node("Tomihari",Node("Tomi",Leaf,Leaf),Node("hari",Leaf,Leaf));;
let tree2 = Node("Akiyoshi",Node("Aki",Leaf,Leaf),Node("yoshi",Leaf,Leaf));;
let tree2 = Node("TomihariAkiyoshi",tree1,tree2);;

type ib_list = INill
|ICons of int *bi_list 
and bi_list = BNill
| BCons of bool*ib_list;;

let i1 = ICons(0,BCons(true,INill));;
let i2 = ICons(1,BCons(false,i1));;
let i3 = ICons(2,BCons(true,i2));;

type iexpr =
| EConstInt of int
| EAdd of iexpr*iexpr 
| ESub of iexpr*iexpr 
| EMul of iexpr*iexpr;; 

let rec eval a =
  match a with
  | EConstInt(n) -> n
  | EAdd(n,m) -> eval n+eval m 
  | ESub (n,m) -> eval n-eval m 
  | EMul (n,m) -> eval n*eval m;; 

let t = ref 0;;
let f x =
  let ans = !t in 
  t:=x;ans;;

