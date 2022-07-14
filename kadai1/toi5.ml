let rec append la lb =
  match la with
  | [] -> lb
  | x::ly -> x::append ly lb;;

let rec filter f l=
  match l with 
    | [] -> []
    | x::lx -> 
      if f x then x::filter f lx
      else filter f lx;;

append [1;2;3] [5;6;7;8];;
filter (fun x -> x > 5) [1;7;2;5;6;8;10];;