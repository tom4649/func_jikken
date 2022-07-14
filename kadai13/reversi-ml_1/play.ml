open Color
open Command


type board = color array array

let init_board () =
  let board = Array.make_matrix 10 10 none in
    for i=0 to 9 do
      board.(i).(0) <- sentinel ;
      board.(i).(9) <- sentinel ;
      board.(0).(i) <- sentinel ;
      board.(9).(i) <- sentinel ;
    done;
    board.(4).(4) <- white;
    board.(5).(5) <- white;
    board.(4).(5) <- black;
    board.(5).(4) <- black;
    board

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ]

let flippable_indices_line board color (di,dj) (i,j) =
  let ocolor = opposite_color color in
  let rec f (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else
      []
  and    g (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else if board.(i).(j) = color then
      r
    else
      [] in
    f (di,dj) (i,j) []



let flippable_indices board color (i,j) =
  let bs = List.map (fun (di,dj) -> flippable_indices_line board color (di,dj) (i+di,j+dj)) dirs in
    List.concat bs

let is_effective board color (i,j) =
  match flippable_indices board color (i,j) with
      [] -> false
    | _  -> true

let is_valid_move board color (i,j) =
  (board.(i).(j) = none) && is_effective board color (i,j)


let doMove board com color =
  match com with
      GiveUp  -> board
    | Pass    -> board
    | Mv (i,j) ->
	let ms = flippable_indices board color (i,j) in
	let _  = List.map (fun (ii,jj) -> board.(ii).(jj) <- color) ms in
	let _  = board.(i).(j) <- color in
	  board

let mix xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)


let valid_moves board color =
  let ls = [1;2;3;4;5;6;7;8] in
  List.filter (is_valid_move board color)
    (mix ls ls)


let count board color =
  let s = ref 0 in
    for i=1 to 8 do
      for j=1 to 8 do
        if board.(i).(j) = color then s := !s + 1
      done
    done;
    !s


let print_board board =
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=1 to 8 do
    print_int j; print_string "|";
    for i=1 to 8 do
      print_color (board.(i).(j)); print_string " "
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"


let report_result board =
  let _ = print_endline "========== Final Result ==========" in
  let bc = count board black in
  let wc = count board white in
    if bc > wc then
      print_endline "*Black wins!*"
    else if bc < wc then
      print_endline "*White wins!*"
    else
      print_endline "*Even*";
    print_string "Black: "; print_endline (string_of_int bc);
    print_string "White: "; print_endline (string_of_int wc);
    print_board board

let fin_eval board color =
  let x = count board color in 
  2*x-64

let next_board board color (i,j) =
  let board_cpy = Array.make_matrix 10 10 none in
    for i=0 to 9 do
      board_cpy.(i) <- Array.copy board.(i);
    done;
  doMove board_cpy (Mv(i,j)) color


  let min_score = -100000
  let max_score = 100000
let rec fin_score_mv_max board color alpha beta num =
  if num  >= 64 then fin_eval board color
  else let vm = valid_moves board color in
  let rec chmax ml ms a b n=
    match ml with
    | [] -> !ms
    | (i,j)::mll ->
      let nb = next_board board color (i,j) in
      let sc = -(fin_score_mv_max nb (opposite_color color) (ref (- !b)) (ref (- !a)) (n+1)) in
      if sc >= !b then sc
      else ((if sc > !ms then ms:= sc else ()) ;(if !a < sc then a:=sc else ());
      chmax mll ms a b n) 
  in let smax = ref min_score in
  chmax vm smax alpha beta num

let evaluate board color(*盤面の評価*)=
let ij_l1=[(1,4);(1,5);(3,4);(3,5);(4,4);(4,5);(4,6);(4,8);(5,5);(5,6);(5,8)] in
let ij_l2=[(2,3);(2,4);(2,5);(2,6);(3,7);(4,7);(5,7);(6,7)] in
let ij_l3=[(1,2);(1,7);(2,8);(7,8)] in
let ij_l4=[(2,2);(2,7);(7,7)] in
let ij_l5=[(1,1);(1,8);(8,8)] in
let ans = ref 0 in
List.iter (fun (i,j) -> if i=j then ans:= !ans -board.(i).(j) else ans:= !ans -board.(i).(j)-board.(j).(i)) ij_l1;
List.iter (fun (i,j) -> if i=j then ans:= !ans -3*board.(i).(j) else ans:= !ans -3*(board.(i).(j)+board.(j).(i))) ij_l2;
List.iter (fun (i,j) -> if i=j then ans:= !ans -board.(i).(j) else ans:= !ans -12*(board.(i).(j)+board.(j).(i))) ij_l3;
List.iter (fun (i,j) -> if i=j then ans:= !ans -board.(i).(j) else ans:= !ans -15*(board.(i).(j)+board.(j).(i))) ij_l4;
List.iter (fun (i,j) -> if i=j then ans:= !ans -board.(i).(j) else ans:= !ans +30*(board.(i).(j)+board.(j).(i))) ij_l5;
if color = white then !ans else - (!ans)

  let num_fin = 53

  let chl_max l f =
    let ll = List.map f l in
    let maxref = ref min_score in let maxid = ref 0 in
    let n = List.length l in
    for i=0 to (n-1) do
      (if (List.nth ll i)> !maxref then (maxref:=(List.nth ll i); maxid := i) else ())
    done;
    List.nth l !maxid


let depth_read = 1


let num_opt_op board color (i,j)=(*(i,j)の手を指したときの相手の手数*)
let nb = next_board board color (i,j) in
  List.length (valid_moves nb (opposite_color color))
let num_opt_self board color =
  List.length (valid_moves board color)

let eval board color =(*次の手がcolorの番の盤面boardでの評価値を返す*)
let n_opt = (ref (-1)) in 
let vm = valid_moves board color in
List.iter (fun (i,j) -> let m = num_opt_op board color (i,j) in if !n_opt < m then n_opt:=m else ()) vm;
let num = count board white + count board black in
(* num_opt_self board color - (64-num)*(!n_opt)+(evaluate board color)  *)
let num = count board white + count board black in
if num< 16 then num_opt_self board color - 60*(!n_opt)+(evaluate board color)
else if num< 40 then num_opt_self board color - 60*(!n_opt)+2*(evaluate board color)
else num_opt_self board color - 10*(!n_opt)+10*(evaluate board color)+(2*(count board color)-num) 



let rec eval_mv_max board color alpha beta d =
  if d<=0 then 
    eval board color
  else let vm = valid_moves board color in
  let rec chmax ml ms a b dep=
    match ml with
    | [] -> !ms
    | (i,j)::mll ->
      let nb = next_board board color (i,j) in
      let sc = -(eval_mv_max nb (opposite_color color) (ref (- !b)) (ref (- !a)) (dep-1)) in
      if sc >= !b then sc
      else ((if sc > !ms then ms:= sc else ()) ;(if !a < sc then a:=sc else ());
      chmax mll ms a b dep) 
  in let smax = ref min_score in
  chmax vm smax alpha beta d




let play board color =
    let ms = valid_moves board color in
      if ms = [] then
        Pass
      else
        let n = count board white + count board black in
        if n>num_fin then
          let (i,j) = chl_max ms (fun x-> - (fin_score_mv_max (next_board board color x) (opposite_color color) (ref min_score) (ref max_score) (n+1)) ) in
          Mv (i,j)
        else 
          let (i,j) = chl_max ms (fun x -> - (eval_mv_max (next_board board color x) (opposite_color color) (ref min_score) (ref max_score) depth_read))  in
        (* let k = Random.int (List.length ms) in
        let (i,j) = List.nth ms k in *)
    Mv (i,j)
  
