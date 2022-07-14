let rec fix f x = f (fix f) x;;

(*求めたい再帰関数が不動点となるような関数**_genを定義し
再帰関数を使わずに関数を実装*)
let sum_gen f n=
  if n=0 then 0
  else n+f (n-1);;

let sum_to_fix = fun x-> fix sum_gen x;;

(*int aがint b以下の整数で割り切れないときにtrueを返す関数sub_prime_fixを実装*)
(*sub_prime_fixとgcd_fixは引数が２つだが、fixに与えることのできる**_genの引数は１つなので
２つの引数をpairによって一つにまとめた*)
let sub_prime_gen f (a,b)=
  if b=1 then true
  else if (a mod b) =0 then false
  else f (a,(b-1));;

let sub_prime_fix = fun (a,b)-> (fix sub_prime_gen (a,b));;
let is_prime_fix n = 
  if n=1 then false
  else sub_prime_fix (n,n-1);;

let gcd_gen f (a,b) =
  if a < b then f (b,a) 
  else if b = 0 then a
  else  f (b, (a mod b)) ;;
  (*引数をpairから２つの引数に戻すために、gcd_fix_subを挟んだ*)
let gcd_fix_sub = fun x-> fix gcd_gen x;;
let gcd_fix a b = gcd_fix_sub (a,b);;
