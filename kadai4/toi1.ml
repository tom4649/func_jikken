type 'a result = Error of string | Ok of 'a
let myDiv x y = if y=0 then Error "Divison by zero" else Ok (x/y)
let  rec eLookup  x l = match l with |[] -> Error("Not found")
                                      |  (a,b)::res -> if x = a then Ok(b) else eLookup x res    
let (>>=) x f =match x with | Error(s) -> Error(s)(*error messageの処理。文字列に限定？*)
                            | Ok(a) -> f a
let lookupDiv kx ky t =
  (eLookup kx t) >>= (fun x ->
  (eLookup ky t) >>= (fun y ->
    myDiv x y))

let table = [("x",6);("y",0);("z",2)];;
lookupDiv "x" "y" table
lookupDiv "x" "z" table
lookupDiv "x" "b" table
lookupDiv "a" "z" table
