type var = int
type const = string
type term = |Const of const | Var of var|Any(*ワイルドカード*) | List of term list|Fun of string*(term list)(*functor 名,複合項*)
type fact = string*(term list)(*述語名,項*)
type rule = fact*(fact list)(*結論,前提*)

type env = rule list
type goal = fact list

exception UError


let rec lookup_var a l =
  match l with
  |[] -> raise UError
  |(x,z)::ll-> if x = a then z else lookup_var a ll 
let rec subst sub a = 
  match a with 
  | Const _ -> a
  | Any -> a
  | List l -> List( List.map (subst sub) l) 
  | Fun (f,l) -> Fun(f,List.map (subst sub) l)
  | Var x -> try lookup_var x sub with UError -> Var x

let rec occur_check a t = 
  match t with
  | Const _ -> true
  | Any -> true
  | List l -> List.for_all (occur_check a) l
  | Fun (f,l) -> List.for_all (occur_check a) l
  | Var x -> if x =a then false else true
let rec make_const l1 l2 = List.map2 (fun a b->(a,b)) l1 l2
let compose s1 s2 =
  let compose_sub (x,t) = (x,subst s1 t) in
  let temp = List.map compose_sub s2 in
  let rec compose_sub2 l ans =
    match l with
    | [] -> ans
    |(x,t)::ll -> if List.for_all (fun (a,_) -> a<>x) temp
      then compose_sub2 ll ((x,t)::ans) else compose_sub2 ll ans in
compose_sub2 s1 temp
let rec unify const =
  match const with 
  | [] -> []
  | (t1,t2)::ll when (t1 = t2 || t1 = Any || t2 = Any) -> unify ll 
  |(t1,t2)::ll ->( (match t1 with 
    |Var x1 -> if occur_check x1 t2 then 
      let sub = subst [(x1,t2)] in 
      let substed = List.map (fun (s1,s2)-> (sub s1, sub s2)) ll in compose (unify substed) [(x1,t2)]
else raise UError
    |List l1 -> (match t2 with | List l2 ->  unify ((make_const l1 l2)@ll)
                               | Var x2 ->
                                            if occur_check x2 t1 then 
                                              let sub = subst [(x2,t1)] in 
                                              let substed = List.map (fun (s1,s2)-> (sub s1, sub s2)) ll in compose (unify substed) [(x2,t1)]
                                            else raise UError
                               | _  -> raise UError) 
    |Fun (f,tl1) -> (match t2 with | Fun (g,tl2) -> unify ((make_const tl1 tl2)@ll)
                                    |Var x2 ->
                                            if occur_check x2 t1 then 
                                              let sub = subst [(x2,t1)] in 
                                              let substed = List.map (fun (s1,s2)-> (sub s1, sub s2)) ll in compose (unify substed) [(x2,t1)]
                                            else raise UError
                                   | _ -> raise UError)
    |_ -> 
      (match t2 with 
      |Var x2 ->
      if occur_check x2 t1 then 
        let sub = subst [(x2,t1)] in 
      let substed = List.map (fun (s1,s2)-> (sub s1, sub s2)) ll in compose (unify substed) [(x2,t1)]
      else raise UError
      | _ -> raise UError)))

let app_rule (fact:fact) (renv:env)=(*factとruleのリストを受け取って制約を解決した代入と追加するゴールを得る関数*)
let rec app_rule_help (p,l) env ans= 
  match env with
  |[]-> ans
  | ((q,terms),facts)::env' ->  if q=p then try (app_rule_help (p,l) env' ((unify (make_const terms l),facts)::ans)) with UError -> app_rule_help (p,l) env' ans
  else app_rule_help (p,l) env' ans in
app_rule_help fact renv []

let sub_fact sub ((p,tl):fact) = ((p,List.map (subst sub) tl):fact)
let make_newgoals (goals,ss) env = (*(ゴール(factのリスト),代入)とruleのリストを受け取って新しいゴールのリストを作る関数*)
let fun_temp (p,tl) = (*goalsの要素を一つ固定したときの関数*)
  let applied_list = app_rule (p,tl) env in (*mguと追加するゴール*)
  let rec temp  ll ans=
  match ll with 
  |[]-> ans
  |(mgu,facts)::ll' ->  
  temp ll' ((List.map (sub_fact mgu) (List.filter (fun fact -> fact<>(p,tl))(facts@goals)),compose mgu ss) ::ans)
  in
temp applied_list [] in
List.concat (List.map fun_temp goals)

let rec print_term t =
  match t with 
  |Const s -> print_string s
  |Var n -> print_int n
  |Any -> print_string "(_)"
  |List l -> print_string "[";List.iter (fun t1 -> print_term t1;print_string", ")l;print_string"]"
  |Fun (f,l) ->print_string f; print_string "(";List.iter (fun t1 -> print_term t1;print_string", ")l;print_string")"

let print_sub sub =
  match sub with
  |[] -> print_string "true";print_newline()
  |_->
  List.iter (fun (x,t) -> print_int x;print_string "=";print_term t;print_string", ") sub;print_newline()

let print_fact (s,tl)=
 print_string s;(List.iter print_term tl)

let print_rule (f,fl) =
  print_fact f;print_string":-";
  List.iter (fun fact->print_fact fact;print_string ",")fl 


let rec  goal_to_q goals q=
  match goals with
   | [] -> ()
   | (facts,sub)::goals' -> 
    if facts = [] then (print_sub sub;goal_to_q goals' q) 
    else ( Queue.add (facts,sub) q;goal_to_q goals' q)
let add_fact goals orig env q=
let rec add_fact_sub goall = 
  match goall with
  | []->  ()
  | (facts,sub)::ll->
    if facts=[] then 
      (let new_facts =List.map (sub_fact sub) orig in
      let old_env = !env in
      let new_rule = List.concat (List.map (fun f-> if (List.exists (fun (a,b)-> a=f && b=[]) old_env) then [] else [(f,[])] ) new_facts) in
      env := (new_rule @ (! env));
      let newgoals = make_newgoals (orig,[]) !env in(*もう一度キューにゴールを追加*)
      goal_to_q newgoals q)
    else ()
  in add_fact_sub goals

let bfs orig env=
  let q = Queue.create() in
  let newgoals = make_newgoals (orig,[]) !env in
  goal_to_q newgoals q;
  while not (Queue.is_empty q) do
    let goal = Queue.take q in 
    let newgoals' = make_newgoals goal !env in
    add_fact newgoals' orig env q;
    goal_to_q newgoals' q;
    flush stdout
  done

let refvar= ref 0
let getnewvar () = let n = !refvar in refvar:=n+1; n


(*  以下は動作例
let f1 = ("male",[Const "kobo"])
  let f2 = ("male",[Const "koji"])
  let f3 = ("male",[Const "iwao"])
  let f4 = ("female",[Const "sanae"])
  let f5 = ("female",[Const "mine"])
  let f6 = ("parent",[Const "kobo";Const "koji"])
  let f7 = ("parent",[Const "kobo";Const "sanae"])
  let f8 = ("parent",[Const "sanae";Const "iwao"])
  let f9 = ("parent",[Const "sanae";Const "mine"])
  let f10 = ("ancestor",[Var 1 ;Var 2 ])
  let f11 = ("parent",[Var 1 ;Var 2 ])
  let f12 = ("ancestor",[Var 3 ;Var 5 ])
  let f13 = ("parent",[Var 3 ;Var 4 ])
  let f14 = ("ancestor",[Var 4 ;Var 5 ])

  let r1 = ((f1,[]):rule)
  let r2 = ((f2,[]):rule)
  let r3 = ((f3,[]):rule)
  let r4 = ((f4,[]):rule)
  let r5 = ((f5,[]):rule)
  let r6 = ((f6,[]):rule)
  let r7 = ((f7,[]):rule)
  let r8 = ((f8,[]):rule)
  let r9 = ((f9,[]):rule)
  let r10 = ((f10,[f11]):rule)
  let r11 = ((f12,[f13;f14]):rule)
  let env = [r1;r2;r3;r4;r5;r6;r7;r8;r9;r10;r11]
  let er1 = ref env

(*
bfs [f1] er1
bfs [("ancestor",[Const "kobo" ;Var 100])] er1
*)

let f15 =( ("nat" ,[Fun ("s",[Var 6])]):fact)
let f16 =( ("nat" ,[Var 6]):fact)
let f17 =( ("nat" ,[Const "z"]):fact)
let r12 = ((f15,[f16]):rule)
let r13 = ((f17,[]):rule)
let env2 =[r12;r13]
let env3 =[r13;r12]
let er = ref env2
let er3= ref env3

bfs [("nat",[Var 100])] er
*)

(*
let f18 =( ("add" ,[Const "z";Var 7;Var 7]):fact)
let f19 =( ("add" ,[Fun ("s" ,[Var 9]);Var 10;Fun ("s" ,[Var 11])]):fact)
let f20 =( ("add" ,[Var 9;Var 10;Var 11]):fact)
let r14 = ((f18,[]):rule)
let r15 = ((f19,[f20]):rule)

let r16 = (( ("p",[]),[("add" ,[Fun ("s" ,[Const "z"]);Var 13;Var 13])]):rule)
let env4 = [r14;r15;r16]
let er4 = ref env4

let _ =bfs [("p",[])] er4
*)


  
  



