module type EQ =
  sig
    type ('a, 'b) equal
    val refl : ('a, 'a) equal
    val symm : ('a, 'b) equal -> ('b, 'a) equal
    val trans : ('a, 'b) equal -> ('b, 'c) equal -> ('a, 'c) equal
    val apply : ('a, 'b) equal -> 'a -> 'b
  end

module Eq : EQ =struct
  type ('a,'b) equal = ('a -> 'b)*('b->'a)
  let refl = 
    let id: 'a. 'a->'a = (fun x ->x )in
    ((id,id):('a,'a)equal)
  let symm (p:('a,'b)equal) = match p with | (f,g) -> ((g,f) :('b,'a)equal)
  let trans (p:('a,'b)equal) (q:('b,'c)equal) = match p with | (f1,g1) ->( match q with | (f2,g2)->
    ((fun x -> f2 (f1 x)),(fun x -> g1 (g2 x))) :('a,'c)equal)
  let apply (p:('a,'b)equal) = match p with | (f,g) -> f
  
end

type 'a value =
  | VBool of (bool, 'a) Eq.equal * bool
  | VInt of (int, 'a) Eq.equal * int

type 'a expr =
  | EConstInt of (int, 'a) Eq.equal * int
  | EAdd of (int, 'a) Eq.equal * (int expr) * (int expr)
  | ESub of (int, 'a) Eq.equal * (int expr) * (int expr)
  | EMul of (int, 'a) Eq.equal * (int expr) * (int expr)
  | EConstBool of (bool, 'a) Eq.equal * bool
  | EEq of (bool, 'a) Eq.equal * (int expr) * (int expr)
  | ELt of (bool, 'a) Eq.equal * (int expr) * (int expr)
  | EIf of bool expr * 'a expr * 'a expr

let rec eval : 'a. 'a expr -> 'a value = fun tar ->
  match tar with
  | EConstInt(e,c) -> VInt(e,c)
  | EAdd(e,exp1,exp2) -> (match eval exp1 with | VInt(e1,c1) -> (match eval exp2 with | VInt(e2,c2) -> VInt(e, c1+c2)
                                                                                      | VBool(e2,b2) -> VInt(e,(c1+((Eq.apply e2) b2))))
                                                | VBool(e1,b1) -> let c1 = ((Eq.apply e1) b1) in  (match eval exp2 with | VInt(e2,c2) -> VInt(e, c1+c2)
                                                                                                                    | VBool(e2,b2) -> VInt(e,(c1+((Eq.apply e2) b2)))))
  | ESub(e,exp1,exp2) -> (match eval exp1 with | VInt(e1,c1) -> (match eval exp2 with | VInt(e2,c2) -> VInt(e, c1-c2)
                                                                                      | VBool(e2,b2) -> VInt(e,(c1-((Eq.apply e2) b2))))
                                                | VBool(e1,b1) -> let c1 = ((Eq.apply e1) b1) in  (match eval exp2 with | VInt(e2,c2) -> VInt(e, c1-c2)
                                                                                                                    | VBool(e2,b2) -> VInt(e,(c1-((Eq.apply e2) b2)))))
  | EMul(e,exp1,exp2) -> (match eval exp1 with | VInt(e1,c1) -> (match eval exp2 with | VInt(e2,c2) -> VInt(e, c1*c2)
                                                                                      | VBool(e2,b2) -> VInt(e,(c1*((Eq.apply e2) b2))))
                                                | VBool(e1,b1) -> let c1 = ((Eq.apply e1) b1) in  (match eval exp2 with | VInt(e2,c2) -> VInt(e, c1*c2)
                                                                                                                    | VBool(e2,b2) -> VInt(e,(c1*((Eq.apply e2) b2)))))
  | EConstBool(e,b) -> VBool(e,b)
  | EEq(e,exp1,exp2) -> (match eval exp1 with | VInt(e1,c1) -> (match eval exp2 with | VInt(e2,c2) -> VBool(e, c1=c2)
                                                                                      | VBool(e2,b2) -> VBool(e,(c1=((Eq.apply e2) b2))))
                                                | VBool(e1,b1) -> let c1 = ((Eq.apply e1) b1) in  (match eval exp2 with | VInt(e2,c2) -> VBool(e, c1=c2)
                                                                                                                        | VBool(e2,b2) -> VBool(e,(c1=((Eq.apply e2) b2)))))
  | ELt(e,exp1,exp2) -> (match eval exp1 with | VInt(e1,c1) -> (match eval exp2 with | VInt(e2,c2) -> VBool(e, c1<c2)
                                                                                      | VBool(e2,b2) -> VBool(e,(c1<((Eq.apply e2) b2))))
                                                | VBool(e1,b1) -> let c1 = ((Eq.apply e1) b1) in  (match eval exp2 with | VInt(e2,c2) -> VBool(e, c1<c2)
                                                                                                                        | VBool(e2,b2) -> VBool(e,(c1<((Eq.apply e2) b2)))))
  | EIf(exp1,exp2,exp3) -> (match eval exp1 with | VBool(e1,b1) -> if b1 then eval exp2 else eval exp3
                                                | VInt(e1,c1) -> let b1 = ((Eq.apply e1) c1) in  if b1 then eval exp2 else eval exp3)
 
(*以下テスト用コード
  let c1 = EConstInt(Eq.refl,1)
  let c2 = EConstInt(Eq.refl,2)
  let ct = EConstBool(Eq.refl,true)
  let add12 = EAdd(Eq.refl,c1,c2)
  eval add12
  let cadd = EAdd(Eq.refl,c1,ct)
let cif = EIf(ct,add12,c2)
eval cif
let clt = ELt(Eq.refl,EMul(Eq.refl,c1,c2),add12)
eval clt
*)                                                                                                                   
