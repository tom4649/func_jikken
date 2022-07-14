open Syntax
open Eval

let print_ans (id,v) =(*read_eval_printで用いるための補助関数*)
  Printf.printf "%s = " id;
  print_value v;
  print_newline ();;

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try 
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (l,newenv) = eval_command env cmd in
  let _ = List.map print_ans l in
  read_eval_print newenv
with 
| Failure s-> (*lexingのエラー*)
  print_string s;
  print_newline ();
  read_eval_print env
|EvalErr -> print_string "Eval Error";(*evalのエラー*)
print_newline ();
read_eval_print env
|Parsing.Parse_error ->print_string "Parsing Error";(*parsingのエラー*)
print_newline ();
read_eval_print env
|_ -> print_string "Error(maybe Parsing Error)";(*不明なエラー(parsingの可能性が高い)*)
print_newline ();
read_eval_print env



let initial_env =
  empty_env
  |> extend "i" (VInt 1)
  |> extend "v" (VInt 5)
  |> extend "x" (VInt 10)

let _ = read_eval_print initial_env
