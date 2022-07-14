let twoo = fun f x -> f(f x);;
twoo (fun y -> y+1) 0;;