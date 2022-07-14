open Syntax
open Eval
open Check
open ConstraintSolver
open Test

let rec read_eval_print env tenv=
  print_string "# ";
  flush stdout;
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  (*let (t,tenv') = infer_cmd cmd tenv in*)
  print_string "start";flush stdout;
  let (t,tenv')=infer_test cmd tenv in
  (let (id, newenv, v) = eval_command env cmd in
  Printf.printf "%s : " id;
  (*print_type t;*)
  print_string " = ";
  (*print_value v;*)
  print_newline ();
  read_eval_print newenv tenv')
(*with TyError -> print_string "Syntax Error\n"; read_eval_print env tenv*)

let initial_env =
  empty_env
  |> extend "i" (VInt 1)
  |> extend "v" (VInt 5)
  |> extend "x" (VInt 10)

let _ = read_eval_print initial_env empty_env
