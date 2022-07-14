open Syntax
open Eval

let print_ans (id,v) =(*read_eval_print内で用いる出力のための補助関数*)
  Printf.printf "%s = " id;
  print_value v;
  print_newline ();;

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (l,newenv) = eval_command env cmd in
  let _ = List.map print_ans l in
  read_eval_print newenv

let initial_env =
  empty_env
  |> extend "i" (VInt 1)
  |> extend "v" (VInt 5)
  |> extend "x" (VInt 10)

let _ = read_eval_print initial_env
