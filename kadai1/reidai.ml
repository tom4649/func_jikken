let circle x =3.14159*.x*.x;;
circle 10.;;
circle 15.0;;

let rec sigma f n = 
  if n=0 then f 0 else f n + sigma f (n-1);; 
  
sigma (fun x -> x*x + x) 10;;

let rec map f x = match x with 
| [] -> [] 
| y::z -> f y :: map f z;;


map (fun x -> x * x) [1; 2; 3];;
