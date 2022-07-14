let rec sum_to n =
  if n = 0 then 0
  else n + sum_to (n-1);;

  (*int aがint b以下の整数で割り切れないときにtrueを返す関数*)
let rec sub_prime a b =
  if b=1 then true
  else if (a mod b) =0 then false
  else sub_prime a (b-1);;
let is_prime n =
    if n=1 then false
    else sub_prime n (n-1);;

let rec gcd a b =
  if a < b then gcd b a 
  else if b = 0 then a
  else  gcd b (a mod b) ;;
