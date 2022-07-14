module type SEMIRING = sig
type t
val add: t->t->t
val mul: t->t->t
val unit: t
val zero: t
end
exception Invalid_Args

module Matrix = (*行列モジュール*)
functor (T:SEMIRING) -> struct
  let  makematrix l =(*多次元配列lを行列に変換*)
  let rec makematrix_sub n l1 ans = (*make_matrixのための補助関数*)
    match l1 with | [] -> ans | ll::res -> if not((List.length ll)= n) then raise Invalid_Args else makematrix_sub n res (ans@[ll]) in
  match l with | [] -> raise Invalid_Args | ll::res -> if ll=[] then raise Invalid_Args else makematrix_sub (List.length ll) l []
  let mul_one_num avec b num = (*ベクトルと行列の指定した列の積を計算する関数*)
    if not(List.length avec = List.length b) then raise Invalid_Args else
    let rec mul_one_num_sub avecres c num1 num2 ans = match avecres with | []-> ans |a::avecres'->mul_one_num_sub avecres' c num1 (num2+1) (T.add ans (T.mul a (List.nth (List.nth c num2)num1))) in 
  mul_one_num_sub avec b num 0 T.zero
  let mul_one avec b = (*ベクトルと行列の積を計算*)
    match b with [] -> raise Invalid_Args | b1::bres -> 
    let m = List.length b1 in
    let rec mul_one_sub avec b (ans:T.t list) num = if num = m then ans else mul_one_sub avec b (ans@[mul_one_num avec b num])(num+1) in
  mul_one_sub avec b [] 0
let mul a b  = (*行列の積を計算*)
match a with |[] -> raise Invalid_Args | avec::res ->
  let rec mul_sub ares b ans = match ares with | [] -> ans | vec::ares' ->
    mul_sub ares' b (ans@[(mul_one vec b)]) in
  mul_sub a b []
let show a n m = (*n行m列の要素を返す関数*)
  if List.length a < (n-1) then raise Invalid_Args else
    let nrow = List.nth a n in
  if List.length nrow < (m-1) then raise Invalid_Args else
    List.nth nrow m 
let unit n =(*n^nの単位行列を返す関数*)
  let unit_vec k =(*k列目が1,他が0のn列ベクトルを返す関数*)
  let rec unit_vec_sub k ans = 
    let m = List.length ans in if m =n then ans else if m=(k-1) then unit_vec_sub k (ans@[T.unit]) else unit_vec_sub k (ans@[T.zero]) in
  unit_vec_sub k [] in
  let rec unit_sub ans = 
    let m=List.length ans in if m=n then ans else unit_sub (ans@[(unit_vec (m+1))]) in  
  unit_sub []
let pow a n =(*行列aのn乗を返す関数*)
  let rec pow_sub k ans =
    if k = 0 then ans else (pow_sub (k-1) (mul a ans)) in
  pow_sub n (unit (List.length a))
end

module Mybool = struct
  type t = bool
  let add a b = a||b
  let mul a b = a&&b
  let unit = true
  let zero = false
end
module BoolMatrix = Matrix(Mybool)

module Trop  = struct
  type t = Int of int| Inf
  let add a b = match a with | Inf ->b | Int(x) ->(match b with | Inf -> Int(x) | Int(y) -> if x<y then Int(x) else Int(y))
  let mul a b = match a with | Inf ->Inf | Int(x) -> (match b with | Inf -> Inf | Int(y) -> Int(x+y))
  let unit = Int(0)
  let zero = Inf
end
module TropMatrix = Matrix(Trop)

(*(*以下テスト用コード*)
let mytrue=Mybool.unit
let myfalse=Mybool.zero
let m1 = BoolMatrix.makematrix([[true;true;false];[false;true;false]])
let m2 = BoolMatrix.makematrix([[true;true];[false;true];[false;false]])
let m3 =BoolMatrix.mul m1 m2
BoolMatrix.makematrix([[true;true];[true];[false;false]])


let a1=TropMatrix.makematrix [[Trop.Int 1;Trop.Int 2;Trop.Int 3];[Trop.Int 4;Trop.Int 5;Trop.Int 6]] 
let a2=TropMatrix.makematrix [[Trop.Int 1;Trop.Int 2];[Trop.Int 3;Trop.Int 4];[Trop.Int 5;Trop.Int 6]]  
let a3 =TropMatrix.mul a1 a2
TropMatrix.mul a1 a3
TropMatrix.unit 4
TropMatrix.pow a3 3

(*(*経路の例*)
let m=[[Trop.Int 0;Trop.Int 1;Trop.Int 5;Trop.Inf];
        [Trop.Inf;Trop.Int 0;Trop.Int 2;Trop.Int 4];
        [Trop.Inf;Trop.Inf;Trop.Int 0;Trop.Int 1];
        [Trop.Inf;Trop.Inf;Trop.Int 7;Trop.Int 0]] in
ToropMatrix.pow m 5;;
*)
*)