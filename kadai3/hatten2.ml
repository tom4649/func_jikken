module type EQ2 =
  sig
    type ('a, 'b) equal
    val refl : ('a, 'a) equal
    val symm : ('a, 'b) equal -> ('b, 'a) equal
    val trans : ('a, 'b) equal -> ('b, 'c) equal -> ('a, 'c) equal
    val apply : ('a, 'b) equal -> 'a -> 'b
    module Lift : functor(F: sig type 'a t end) -> sig
    val f : ('a,'b) equal -> ('a F.t , 'b F.t) equal
  end
  end;;

  module Eq2 : EQ2 =
  struct 
  module type EqTC = sig
    type a and b
    val f:a->b
    val g:b->a
    module Cast : functor(F:sig type 'a t end) -> sig 
    type cast_a and cast_b
    val cast_f: a F.t -> b F.t
    val cast_g: b F.t -> a F.t
    end 
  end
  
    type ('a,'b) equal = (module EqTC with type a = 'a and type b = 'b) 
    let refl  (type s): (s,s) equal=
      (module struct 
    type a = s and b = s
    let f a = a
    let g a = a
    module Cast (F:sig type 'a t  end) = struct
    type cast_a = s F.t and cast_b = s F.t 
    let cast_f a = a
    let cast_g a = a 
  end
    end : EqTC with type a = s and type b = s
    ) 
    let symm (type s )( type t )(p: (s,t) equal) =
     let module  Pv = (val p: EqTC with type a = 'a and type b = 'b) in
    ((module struct 
  type a = Pv.b and b = Pv.a
  let f = Pv.g
  let g = Pv.f
  module Cast (F:sig type 'a t  end) = struct
    module P = Pv.Cast(F)
  type cast_a = t F.t and cast_b = s F.t 
  let cast_f a = P.cast_g a
  let cast_g a =P.cast_f a
end
  end : EqTC with type a = t and type b = s
  ) : (t,s)equal) 

  let trans (type s) (type t) (type u) (p:(s,t)equal) (q:(t,u)equal) =
    let module  Pv = (val p: EqTC with type a = 'a and type b = 'b) in
    let module  Qv = (val q: EqTC with type a = 'a and type b = 'b) in
    ((module struct 
  type a = Pv.a and b = Qv.b
  let f x= Qv.f @@ Pv.f @@ x
  let g x= Pv.g @@ Qv.g @@ x
  module Cast (F:sig type 'a t  end) = struct
    type cast_a = s F.t and cast_b = u F.t 
    module P = Pv.Cast(F)
    module Q = Qv.Cast(F)
  let cast_f a = Q.cast_f @@ P.cast_f @@ a
  let cast_g a = P.cast_g @@ Q.cast_g @@ a
end
  end : EqTC with type a = s and type b = u
  ) : (s,u)equal) 
 
    
let apply (type s) (type t)(p:(s,t)equal) = 
  let module  Pv = (val p: EqTC with type a = 'a and type b = 'b) in Pv.f

module Lift = functor(F: sig type 'a t end) -> struct
let f (type s)(type t) (p:(s,t)equal )=
let module  Pv = (val p: EqTC with type a = s and type b = t) in
let module P = Pv.Cast(F) in
((module struct 
type a = s F.t and b =t F.t
let f = P.cast_f
let g = P.cast_g
module Cast (G:sig type 'a t  end) = struct
type cast_a = s F.t G.t and cast_b = t F.t G.t
module PP = Pv.Cast(struct type 'a t = ('a F.t) G.t end)
let cast_f a = PP.cast_f a
let cast_g a =PP.cast_g a
end
end : EqTC with type a = s F.t and type b = t F.t): (s F.t,t F.t)equal)

    end
  end;;
  
(*(*以下テスト用コード*)
(*Eq2を用いて言語を定義*)
type 'a value =
  | VBool of (bool, 'a) Eq2.equal * bool
  | VInt of (int, 'a) Eq2.equal * int

type 'a expr =
  | EConstInt of (int, 'a) Eq2.equal * int
  | EAdd of (int, 'a) Eq2.equal * (int expr) * (int expr)
  | ESub of (int, 'a) Eq2.equal * (int expr) * (int expr)
  | EMul of (int, 'a) Eq2.equal * (int expr) * (int expr)
  | EConstBool of (bool, 'a) Eq2.equal * bool
  | EEq of (bool, 'a) Eq2.equal * (int expr) * (int expr)
  | ELt of (bool, 'a) Eq2.equal * (int expr) * (int expr)
  | EIf of bool expr * 'a expr * 'a expr

let rec eval : 'a. 'a expr -> 'a value = fun tar ->
  match tar with
  | EConstInt(e,c) -> VInt(e,c)
  | EAdd(e,exp1,exp2) -> (match eval exp1 with | VInt(e1,c1) -> (match eval exp2 with | VInt(e2,c2) -> VInt(e, c1+c2)
                                                                                      | VBool(e2,b2) -> VInt(e,(c1+((Eq2.apply e2) b2))))
                                                | VBool(e1,b1) -> let c1 = ((Eq2.apply e1) b1) in  (match eval exp2 with | VInt(e2,c2) -> VInt(e, c1+c2)
                                                                                                                    | VBool(e2,b2) -> VInt(e,(c1+((Eq2.apply e2) b2)))))
  | ESub(e,exp1,exp2) -> (match eval exp1 with | VInt(e1,c1) -> (match eval exp2 with | VInt(e2,c2) -> VInt(e, c1-c2)
                                                                                      | VBool(e2,b2) -> VInt(e,(c1-((Eq2.apply e2) b2))))
                                                | VBool(e1,b1) -> let c1 = ((Eq2.apply e1) b1) in  (match eval exp2 with | VInt(e2,c2) -> VInt(e, c1-c2)
                                                                                                                    | VBool(e2,b2) -> VInt(e,(c1-((Eq2.apply e2) b2)))))
  | EMul(e,exp1,exp2) -> (match eval exp1 with | VInt(e1,c1) -> (match eval exp2 with | VInt(e2,c2) -> VInt(e, c1*c2)
                                                                                      | VBool(e2,b2) -> VInt(e,(c1*((Eq2.apply e2) b2))))
                                                | VBool(e1,b1) -> let c1 = ((Eq2.apply e1) b1) in  (match eval exp2 with | VInt(e2,c2) -> VInt(e, c1*c2)
                                                                                                                    | VBool(e2,b2) -> VInt(e,(c1*((Eq2.apply e2) b2)))))
  | EConstBool(e,b) -> VBool(e,b)
  | EEq(e,exp1,exp2) -> (match eval exp1 with | VInt(e1,c1) -> (match eval exp2 with | VInt(e2,c2) -> VBool(e, c1=c2)
                                                                                      | VBool(e2,b2) -> VBool(e,(c1=((Eq2.apply e2) b2))))
                                                | VBool(e1,b1) -> let c1 = ((Eq2.apply e1) b1) in  (match eval exp2 with | VInt(e2,c2) -> VBool(e, c1=c2)
                                                                                                                        | VBool(e2,b2) -> VBool(e,(c1=((Eq2.apply e2) b2)))))
  | ELt(e,exp1,exp2) -> (match eval exp1 with | VInt(e1,c1) -> (match eval exp2 with | VInt(e2,c2) -> VBool(e, c1<c2)
                                                                                      | VBool(e2,b2) -> VBool(e,(c1<((Eq2.apply e2) b2))))
                                                | VBool(e1,b1) -> let c1 = ((Eq2.apply e1) b1) in  (match eval exp2 with | VInt(e2,c2) -> VBool(e, c1<c2)
                                                                                                                        | VBool(e2,b2) -> VBool(e,(c1<((Eq2.apply e2) b2)))))
  | EIf(exp1,exp2,exp3) -> (match eval exp1 with | VBool(e1,b1) -> if b1 then eval exp2 else eval exp3
                                                | VInt(e1,c1) -> let b1 = ((Eq2.apply e1) c1) in  if b1 then eval exp2 else eval exp3)
  
  let c1 = EConstInt(Eq2.refl,1)
  let c2 = EConstInt(Eq2.refl,2)
  let ct = EConstBool(Eq2.refl,true)
  let add12 = EAdd(Eq2.refl,c1,c2) 
  eval add12;;
  let cadd = EAdd(Eq2.refl,c1,ct)(*エラー発生*)
let cif = EIf(ct,add12,c2)
eval cif ;; 
let clt = ELt(Eq2.refl,EMul(Eq2.refl,c1,c2),add12)
eval clt;;

*)
