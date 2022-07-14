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
      