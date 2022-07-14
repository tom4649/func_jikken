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

n2i (i2n 5);;
i2n 0;;
i2n (-1);;
n2i (add (i2n 8) (i2n 5));;
n2i (add Z (i2n 5));;
n2i (sub (i2n 8) (i2n 5));;
n2i (sub Z (i2n 5));;
n2i (mul (i2n 8) (i2n 5));;
n2i (mul (i2n 5) Z);;
n2i (mul Z (i2n 5));;
n2i (pow (i2n 2) (i2n 5));;
n2i (pow Z (i2n 5));;
n2i (pow Z Z);;

