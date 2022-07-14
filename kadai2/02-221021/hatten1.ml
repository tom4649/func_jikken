let fix f x = 
  let  l = ref [] in 
  let ans = fun g y -> 
    match !l with 
    |h::_ -> g (h g) y 
    | _ -> assert false(*絶対に発生しない*)in
  l:=[ans]; ans f x;;
