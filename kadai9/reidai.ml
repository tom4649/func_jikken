type name = string 

type expr =
  | EConstInt  of int
  | EConstBool of bool
  | EVar       of name
  | EAdd       of expr * expr
  | ESub       of expr * expr
  | EMul       of expr * expr
  | EDiv       of expr * expr
  | EEq        of expr * expr
  | ELt        of expr * expr
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | EFun       of name * expr
  | EApp       of expr * expr
  | ELetRec    of name * name * expr * expr
  | EPair of expr*expr


type value =
  | VInt  of int
  | VBool of bool
  | VFun of name*expr*env
  | VRecFun of name*name*expr*env
  | VPair of value*value
  |VNil
  |VCons of value*value
and 
env = (name * value) list

type pattern = PInt of int | PBool of bool
|PVar of name 
|PPair of pattern*pattern
|PNil|PCons of pattern*pattern

type 'a option = None | Some of 'a
let rec find_match p v =
  match p with
  | PInt n -> (match v with | VInt m -> if n = m then Some [] else None |_ -> None)
  | PBool b -> (match v with | VBool c -> if b = c then Some [] else None |_ -> None)
  | PVar x -> Some[(x,v)]
  | PPair(p1,p2) ->(match v with 
  | VPair(v1,v2) -> 
    let l1 = find_match p1 v1 in let l2= find_match p2 v2 in 
  (match l1 with
  | Some a1 -> (match l2 with | Some a2 -> Some (a1@a2) | _ -> None)
  | _ -> None)
  | _ -> None)
  | PNil -> (match v with | VNil -> Some [] | _ -> None)
  | PCons (p1,p2) -> (match v with 
  | VCons(v1,v2) -> 
    let l1 = find_match p1 v1 in let l2= find_match p2 v2 in 
  (match l1 with
  | Some a1 -> (match l2 with | Some a2 -> Some (a1@a2) | _ -> None)
  | _ -> None)
  | _ -> None)

