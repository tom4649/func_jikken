type nat = Z | S of nat;;
(*いずれも再帰関数を用いることで実装した*)
let rec add a b =
  match a with
  | Z -> b
  | S(t) -> S(add t b);;
let rec sub a b =
  match b with
  | Z -> a
  | S(t) -> match a with
    | Z -> Z
    | S(u) -> sub u t;;

let rec mul a b =
  match b with
  | Z -> Z
  | S(u) -> add a (mul a u);;

  let rec pow x n =
    match n with
    | Z -> S(Z)
    | S(m) -> mul x (pow x m);;

let rec n2i n =
  match n with
  | Z -> 0
  | S(m) -> 1+ (n2i m);;

let rec i2n x =
  if x > 0 then S(i2n (x-1)) else Z;;
