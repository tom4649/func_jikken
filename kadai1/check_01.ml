module Util = struct
  let last_opt xs = List.fold_left (fun _ x -> Some x) None xs

  let rec drop n = function
    | _ :: l when n > 0 -> drop (n-1) l
    | l -> l

  let rec input_lines cin =
    let s = input_line cin in
    let ss =
      try
        input_lines cin
      with End_of_file -> []
    in
    s :: ss

  let starts_with s1 s2 =
    let len1 = String.length s1 in
    let len2 = String.length s2 in
    if len1 < len2 then
      false
    else
      String.sub s1 0 len2 = s2

  let ends_with s1 s2 =
    let len1 = String.length s1 in
    let len2 = String.length s2 in
    if len1 < len2 then
      false
    else
      String.sub s1 (len1-len2) len2 = s2

  let is_int_char c =
    let n = int_of_char c in
    int_of_char '0' <= n && n <= int_of_char '9'

  let time () =
    let {Unix.tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; _} = Unix.localtime @@ Unix.time() in
    Format.sprintf "%04d%02d%02d%02d%02d%02d" (tm_year+1900) (tm_mon+1) tm_mday tm_hour tm_min tm_sec
end

module Config = struct
  let no = 1
  let version = "4.13.1"
  let dir = "_fl_tmp_" ^ Util.time()
  let file_dir = ref ""
  let id = ref ""
  let num_toi = 7
  let num_hatten = 3
  let force = ref false
  let jp = ref true
  let file = ref ""
end

type t = Toi of int | Hatten of int
type item = Val of string | Type of string * string
type error =
  | Version_mismatch
  | Cannot_extract
  | File_name_invalid of string
  | Directory_not_found of string
  | File_not_found of string
  | Value_not_found of string
  | Type_mismatch of string * string * string
  | Unknown_error of string

let report_name = "report"
let report_exts = ["txt"; "md"; "pdf"]

let assiginments =
  [Toi 1, [Type("sum_to", "int -> int");
           Type("is_prime", "int -> bool");
           Type("gcd", "int -> int -> int")];
   Toi 2, [Val "twice";
           Val "repeat"];
   Toi 3, [Type("sum_to_fix", "int -> int");
           Type("is_prime_fix", "int -> bool");
           Type("gcd_fix", "int -> int -> int")];
   Toi 4, [Val "fold_left";
           Val "fold_right"];
   Toi 5, [Type("append", "'a list -> 'a list -> 'a list");
           Type("filter", "('a -> bool) -> 'a list -> 'a list")];
   Toi 6, [Type("append_left", "'a list -> 'a list -> 'a list");
           Type("filter_left", "('a -> bool) -> 'a list -> 'a list");
           Type("append_right", "'a list -> 'a list -> 'a list");
           Type("filter_right", "('a -> bool) -> 'a list -> 'a list")];
   Toi 7, [Type("perm", "'a list -> 'a list list")];
   Hatten 1, [Val "reverse";
              Val "reverse_right"];
   Hatten 2, [Val "fold_left_by_right";
              Val "fold_right_by_left"];
   Hatten 3, [Val "add";
              Val "mul";
              Val "sub"]]

let filename_of = function
  | Toi n -> Format.sprintf "toi%d.ml" n
  | Hatten n -> Format.sprintf "hatten%d.ml" n

let subject_of t =
  match t, !Config.jp with
  | Toi n, true -> "問" ^ string_of_int n
  | Toi n, false -> "Toi " ^ string_of_int n
  | Hatten n, true -> "発展" ^ string_of_int n
  | Hatten n, false -> "Hatten " ^ string_of_int n

let message_of e =
  match e, !Config.jp with
  | Version_mismatch, true -> Format.sprintf "OCaml %s で実行してください" Config.version
  | Version_mismatch, false -> Format.sprintf "Execute this program with OCaml %s." Config.version
  | Cannot_extract, true -> Format.sprintf "入力ファイルの展開に失敗しました"
  | Cannot_extract, false -> Format.sprintf "Cannot extract the input file"
  | File_name_invalid f, true -> Format.sprintf "ファイル名(%s)が不正です" f
  | File_name_invalid f, false -> Format.sprintf "Filename (%s) invalid" f
  | Directory_not_found f, true -> Format.sprintf "ディレクトリ(%s)が見つかりません" f
  | Directory_not_found f, false -> Format.sprintf "Directory (%s) not found" f
  | File_not_found f, true -> Format.sprintf "ファイル(%s)が見つかりません" f
  | File_not_found f, false -> Format.sprintf "File (%s) not found" f
  | Value_not_found v, true -> Format.sprintf {|"%s"が見つかりません|} v
  | Value_not_found v, false -> Format.sprintf {|"%s" not found|} v
  | Type_mismatch(v,_,_), true -> Format.sprintf "%s の型が合っていません" v
  | Type_mismatch(v,_,_), false -> Format.sprintf "Type of %s is mismatched" v
  | Unknown_error s, true -> Format.sprintf "不明なエラー (%s)" s
  | Unknown_error s, false -> Format.sprintf "Unknown error (%s)" s

module Args = struct
  let options =
    ["-f", Arg.Set Config.force, "";
     "-en", Arg.Clear Config.jp, ""]

  let set_file filename =
    if !Config.file <> "" then
      begin
        if !Config.jp then
          Printf.printf "ファイル引数は一つまでです\n"
        else
          Printf.printf "Only one file argument is allowed.\n";
        exit 1
      end;
    Config.file := filename

  let usage = Format.sprintf "Usage: ocaml <stdlib>/unix.cma check%02d.ml %02d-XXXXXX.zip" Config.no Config.no

  let parse () = Arg.parse options set_file usage
end

let init () =
  Args.parse();
  if not !Config.force && Sys.ocaml_version <> Config.version then
    Error Version_mismatch
  else
    begin
      if not @@ Sys.file_exists Config.dir then Sys.mkdir Config.dir 0o755;
      Ok ()
    end

let finalize () =
  if Sys.file_exists Config.dir then
  let r = Sys.command @@ Format.sprintf "rm -r %s" Config.dir in
  assert (r = 0)

let show_error_and_exit = function
  | Ok _ -> ()
  | Error e ->
      Printf.printf "%s\n" (message_of e);
      finalize ();
      exit 1

let check_filename ?ext s =
  match
    match ext with
    | None -> Some s
    | Some e when Util.ends_with s ("."^e) -> Some String.(sub s 0 (length s - length e - 1))
    | _ -> None
  with
  | None -> None
  | Some s ->
      if String.length s = 9 && Util.starts_with s (Format.sprintf "%02d-" Config.no) then
        let s' = String.sub s 3 (String.length s - 3) in
        if Seq.fold_left (fun acc c -> acc && Util.is_int_char c) true (String.to_seq s') then
          Some s'
        else
          None
      else
        None

let check_file_organization () =
  match check_filename ~ext:"zip" !Config.file with
  | None -> Error (File_name_invalid !Config.file)
  | Some id ->
      Config.id := id;
      let r = Sys.command @@ Format.sprintf "unzip -q -d %s %s" Config.dir !Config.file in
      if (r <> 0) then
        Error Cannot_extract
      else
        let dir = Config.dir ^ "/" ^ Filename.chop_extension !Config.file in
        Config.file_dir := dir;
        if not (Sys.file_exists dir && Sys.is_directory dir) then
          Error (Directory_not_found (Filename.chop_extension !Config.file))
        else if not @@ List.exists (fun ext -> Sys.file_exists (Format.sprintf "%s/%s.%s" dir report_name ext)) report_exts then
          Error (File_not_found (report_name ^ ".*"))
        else
          Ok ()

let rec remove_indent acc_rev ss =
  match ss with
  | [] -> List.rev acc_rev
  | ""::ss' -> remove_indent acc_rev ss'
  | s::ss' when Util.starts_with s "    " && acc_rev <> [] ->
      let s' = List.hd acc_rev ^ String.sub s 3 (String.length s - 3) in
      let acc_rev' = s' :: List.tl acc_rev in
      remove_indent acc_rev' ss'
  | s::ss' when s.[0] = '=' && acc_rev <> [] ->
      let s' = List.hd acc_rev ^ " " ^ s in
      let acc_rev' = s' :: List.tl acc_rev in
      remove_indent acc_rev' ss'
  | s::ss' -> remove_indent (s::acc_rev) ss'

let eval_file filename v =
  let cmd = Format.sprintf "ocaml -noprompt -color never -init %s" filename in
  let cin,cout = Unix.open_process cmd in
  output_string cout (v ^ ";;\n");
  close_out cout;
  let s =
    Util.input_lines cin
    |> remove_indent []
    |> Util.last_opt
    |> Option.get
  in
  close_in cin;
  if Util.starts_with s "Error: Unbound value " then
    Error (Value_not_found v)
  else if Util.starts_with s "- : " then
    Ok s
  else
    Error (Unknown_error s)

let check_item filename item =
  match item with
  | Val v ->
      begin
        match eval_file filename v with
        | Ok _ -> None
        | Error e -> Some e
      end
  | Type(v, ty) ->
      begin
        match eval_file filename v with
        | Ok s when Util.starts_with s ("- : "^ty^" = ") -> None
        | Ok s -> Some (Type_mismatch(v, ty, s))
        | Error e -> Some e
      end

let check_file t items =
  let filename = !Config.file_dir ^ "/" ^ filename_of t in
  if not @@ Sys.file_exists filename then
    [File_not_found (Format.sprintf "%d-%s/%s" Config.no !Config.id (filename_of t))]
  else
    items
    |> List.filter_map (check_item filename)

let show_results (t, items, result) =
  Printf.printf "[%s] " (subject_of t);
  if result = [] then
    Printf.printf "OK"
  else
    result
    |> List.map message_of
    |> String.concat ", "
    |> Printf.printf "NG: %s";
  Printf.printf "\n"

let main () =
  init()
  |> show_error_and_exit;

  if !Config.file = "" then (Printf.printf "%s\n" Args.usage; exit 1);

  check_file_organization()
  |> show_error_and_exit;

  assiginments
  |> List.map (fun (t,items) -> t, items, check_file t items)
  |> List.iter show_results;
  finalize()

let () = if not !Sys.interactive then main()
