open Syntax
open Eval
open Check
open ConstraintSolver
let rec read_eval_print env eenv tenv=(*eenvとして例外の名前を保持しておくリストを追加*)
  print_string "# ";
  flush stdout;
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (t,tenv') = infer_cmd cmd tenv in
  try (let (id, newenv, v,eenv') = eval_command env eenv cmd in
  if id ="exception" then (print_string "exception ";print_value v;print_newline(); read_eval_print newenv eenv' tenv')
  else 
  (match v with 
  | VExcep a -> print_string ("Exception: "^a);print_newline(); read_eval_print env eenv' tenv'
  |_ ->
  Printf.printf "%s : " id;
  print_type t;
  print_string " = ";
  print_value v;
  print_newline ();
  read_eval_print newenv eenv' tenv'))
with |TyError -> print_string "Syntax Error\n"; read_eval_print env eenv tenv
    |EvalErr ->print_string "Eval Error\n"; read_eval_print env eenv tenv
let initial_env =
  empty_env
  |> extend "i" (VInt 1)
  |> extend "v" (VInt 5)
  |> extend "x" (VInt 10)

let _ = read_eval_print initial_env [] []
