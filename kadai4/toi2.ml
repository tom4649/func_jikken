type 'a m = 'a list
let (>>=) x f = List.concat(List.map f x) 
let return x =[x] 
let guard b = if b then return () else [] 
let fukumen1 =
  let f ba na si mo nn = (ba*100+na*11)*2=si*1000+na*100+mo*10+nn in
  [0;1;2;3;4;5;6;7;8;9] >>= (fun ba ->
  [0;1;2;3;4;5;6;7;8;9] >>= (fun na ->
  [0;1;2;3;4;5;6;7;8;9] >>= (fun si -> 
  [0;1;2;3;4;5;6;7;8;9] >>= (fun mo ->
  [0;1;2;3;4;5;6;7;8;9] >>= (fun nn ->
    guard(f ba na si mo nn) >>= (fun _ ->
      return (ba,na,si,mo,nn)))))));;

let fukumenn2 =
    let f s e n d m o r y = (s*1000+e*100+n*10+d)+(m*1000+o*100+r*10+e)=(m*10000+o*1000+n*100+e*10+y )in
    let myguard b = if b then [] else [0;1;2;3;4;5;6;7;8;9] in(*略記のための関数myguard*)
    [1;2;3;4;5;6;7;8;9] >>= (fun s ->
    [1;2;3;4;5;6;7;8;9] >>= (fun m ->
    myguard(s=m || s+m > 10*m+9) >>= (fun o -> 
    myguard(s=o || m=o || s+m>(m*10+o))>>= (fun e ->
    myguard(s=e || m=e || o=e ||10*s+e+10*m+o>100*m+10*o+9) >>= (fun d ->
    myguard(s=d || m=d || o=d || e=d || (s*1000+e*100+0*10+d)+(m*1000+o*100+0*10+e)>(m*10000+o*1000+9*100+e*10+9 )) >>= (fun y -> 
    myguard(s=y || m=y || o=y || d=y || e=y ||not((d+e) mod 10=y)||(s*1000+e*100+0*10+d)+(m*1000+o*100+0*10+e)>(m*10000+o*1000+9*100+e*10+y )) >>= (fun r ->
    myguard(s=r || m=r || o=r || d=r || e=r || y=r || 100*s+10*e+0+100*m+10*o+r>1000*m+100*o+10*9+e) >>= (fun n ->
    if s=n || m=n || o=n || d=n || e=n || y=n || r=n || not ((10*n+d+10*r+e)mod 100 = (10*e+y))  then [] else guard(f s e n d m o r y) >>= (fun _ ->
        return (s,e,n,d,m,o,r,y))))))))));;


  