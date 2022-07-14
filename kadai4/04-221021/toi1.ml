type ('a,'b) result = Error of 'b | Ok of 'a
let myDiv x y = if y=0 then Error "Division by Zero" else Ok (x/y)
let  rec eLookup  x l = match l with |[] -> Error("Not found")
                                      |  (a,b)::res -> if x = a then Ok(b) else eLookup x res    
let (>>=) x f =match x with | Error(s) -> Error(s)
                            | Ok(a) -> f a
let lookupDiv kx ky t =
  (eLookup kx t) >>= (fun x ->
  (eLookup ky t) >>= (fun y ->
    myDiv x y))
