type 'a m = 'a*string
let (>>=) x f = match x with 
| (a,s) ->( match f a with (b,t)-> (b,s^t))
let return a = (a,"")
let writer s = ((),s)
