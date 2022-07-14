let fix f x = 
  let exception Temp_Error in
  let  l = ref [] in 
  let ans = fun g y -> 
    match !l with 
    |h::_ -> g (h g) y 
    | _ -> raise Temp_Error(*絶対に発生しない*)in
  l:=[ans]; ans f x;;


