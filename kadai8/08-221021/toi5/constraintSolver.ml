type tyvar =int
type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
type subst = (tyvar*ty)list
type type_schema = tyvar list*ty 
type type_env =(tyvar * type_schema) list

exception TyError
let rec lookup a l =
  match l with
  |[] -> raise TyError
  |(x,z)::ll-> if x = a then z else lookup a ll 

let rec ty_subst sub a =(*型変数の代入後の値を返し、型変数が型代入に登録されていなければ例外TyErrorを返す関数*)
  match a with
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVar x -> (try (lookup x sub) with TyError -> TyVar x)
  | TyFun(a1,a2) -> TyFun(ty_subst sub a1,ty_subst sub a2)

let compose s1 s2 =
  let compose_sub (x,t) = (x,ty_subst s1 t)in
  let temp = List.map compose_sub s2 in
  let rec compose_sub2 l ans =(*tempに登録されていない変数のリストを返す関数*)
    match l with
    | [] -> ans
    | (x,t)::ll -> if List.for_all (fun (a,_) -> a<>x) temp(*xが全てのtempに登録された変数と異なっていたらansに追加*)
      then compose_sub2 ll ((x,t)::ans) else compose_sub2 ll ans in
  compose_sub2 s1 temp

let rec occur_check a t=(*型変数aが型tの自由変数に含まれなければ真を返す関数*)
 match t with
 | TyInt -> true
 | TyBool -> true
 | TyFun(t1,t2) -> occur_check a t1 && occur_check a t2
 | TyVar x -> a<>x
let rec unify const =
  match const with
  | [] -> []
  | (s1,s2)::l when s1=s2 ->unify l
  |(s1,s2)::l ->(match s1 with 
  | TyFun(a1,b1) ->(match s2 with
  |TyFun(a2,b2)-> unify([(a1,a2);(b1,b2)]@l)
  |TyVar (a) -> if (occur_check a s1) then
    let substed = List.map (fun (t1,t2) -> (ty_subst [(a,s1)] t1,ty_subst [(a,s1)] t2)) l in compose (unify substed) [(a,s1)]
  else raise TyError
  |_ -> raise TyError)
  |TyVar(a) -> if (occur_check a s2) then
    let substed = List.map (fun (t1,t2) -> (ty_subst [(a,s2)] t1,ty_subst [(a,s2)] t2)) l in compose (unify substed) [(a,s2)]
  else raise TyError
  | _ -> (match s2 with
  | TyVar (a) -> if (occur_check a s1) then
    let substed = List.map (fun (t1,t2) -> (ty_subst [(a,s1)] t1,ty_subst [(a,s1)] t2)) l in compose (unify substed) [(a,s1)]
  else raise TyError
  |_ -> raise TyError))

  let get_type_vars t =(*型に含まれる型変数のリストを返す関数*)
    let rec get_fv_sub t0 ans =
      match t0 with
      | TyVar(x) -> if List.mem x ans then ans else x::ans
      | TyBool -> ans
      | TyInt-> ans
      | TyFun(t1,t2) ->  get_fv_sub t2 (get_fv_sub t1 ans) in
      get_fv_sub t []

let get_var_fixed (l,t) =(*型スキームの任意でない型変数を返す関数*)
  let fv = get_type_vars t in
  List.filter (fun tvar -> not (List.mem tvar l)) fv
let get_var_env env= (*型環境に含まれる型変数を返す関数*)
    let rec get_var_env_sub envv ans = match envv with
    | [] -> ans 
    | (_,sch)::ll -> let fv =get_var_fixed sch in
    let add = List.filter (fun x-> not (List.mem x ans)) fv in
    get_var_env_sub ll add@ans in
    get_var_env_sub env []
let get_var_forall env t =(*型tの型環境に含まれない変数のリストを返す関数*)
    let fv = get_type_vars t in
    let fv_env = get_var_env env in
    List.filter (fun x-> not(List.mem x fv_env)) fv 

let generalize env t =(*型tを型環境envのもとで型スキームに拡張する関数*)
     (get_var_forall env t,t)

let counter = ref 0
let new_tyvar() =(*フレッシュな型変数を返す関数*)
    let v = !counter in
  counter:=v+1; v
let instantiate (l,t) = (*型スキームの任意の型に新しい型変数を割り当てて型に変換*)
  let var_env = List.map (fun var -> (var,TyVar (new_tyvar()))) l in
  ty_subst var_env t

let sub_for_schema sub (l,_) =(*型スキーマの任意の型に含まれていない代入を返す関数*)
  let rec sub_help subb ans = match subb with
  | [] -> ans 
  |(var,t)::ll ->if List.mem var l then sub_help ll ans
  else sub_help ll ((var,t)::ans ) in
  sub_help sub []

let sch_subst sub (l,t) =(*型スキームに代入を適用する関数*)
  let sub_scheme = sub_for_schema sub (l,t) in
  (l,ty_subst sub_scheme t)
let env_subst sub env = (*環境に代入する関数*)
  List.map (fun (var,sch) ->(var ,sch_subst sub sch)) env

let rec print_type t = match t with
  | TyBool -> print_string "bool"
  | TyInt -> print_string "int"
  | TyVar a -> print_string ("a"^(string_of_int a))
  | TyFun(t1,t2) -> print_string "(";print_type t1;print_string" -> ";print_type t2;print_string ")"


(*(*問5の動作例*)
let t1 = TyFun(TyFun(TyBool,TyVar 0),TyFun(TyFun(TyVar 1,TyVar 2),TyVar 0))
let t2 = TyFun(TyFun(TyBool,TyVar 0),TyFun(TyFun(TyVar 3,TyVar 5),TyVar 0))
let t3 = TyFun(TyFun(TyBool,TyVar 1),TyFun(TyFun(TyVar 2,TyVar 3),TyVar 4))
let sch1 = ([0;1],t1)
let sch2 = ([0],t2)
let tenv = [("a1",sch1);("a2",sch2)]
let sub1= [(0,TyInt);(1,TyBool);(2,TyVar 3)]*)