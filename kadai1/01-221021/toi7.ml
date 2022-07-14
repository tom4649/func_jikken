let rec insert la lb c =
  match lb with
  | [] -> [la@[c]] 
  | b::l -> (la@(c::lb))::insert (la@[b]) l c;;

let rec app c ll=
  match ll with
  | [] -> []
  | l::lla -> (insert [] l c)@ (app c lla);;

let rec perm l =
  match l with
  | [] -> []
  |a::[]-> [[a]]
  | a::la -> app a (perm la);; 
