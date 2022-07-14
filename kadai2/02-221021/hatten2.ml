type false_t = {t: 'a. 'a}
type 'a not_t = 'a -> false_t
type ('a,'b) and_t = 'a*'b
type ('a,'b) or_t = L of 'a | R of 'b;;
let prop1 f g a = g (f a);;(*1*)
let prop2 (x:('a, ('b,'c) and_t)or_t) = match x with L(a) -> (L(a),L(a))(*2*)
                                                |R((b,c)) -> ((R(b),R(c)));;
let prop3 x = match x with (L(a),_) -> L(a)(*3*)
                        |(_,L(a)) -> L(a)
                        |(R(b),R(c)) -> R((b,c):(('b,'c)and_t));;
let rec (callcc :(('a -> false_t) ->'a) ->'a) = fun f -> callcc f;;
let prop_cc4 f g =(*4*)
  let h b = match b with |L(a) -> f a
                        | R(a') -> g a' in
  let v = fun s->
    let t = fun a -> L(a) in
    let u = fun a -> s(t a) in
    R(u) in
h (callcc v);;
(*5は定義できない*)
let prop_cc6 (f:('a->'b) -> 'a) =(*6*)
  let g x = f (fun a -> ((x a).t : 'b)) in
    callcc g;;