
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | TIMES
    | THEN
    | SEMISEMI
    | RPAR
    | REC
    | PLUS
    | MINUS
    | LT
    | LPAR
    | LET
    | INT of (
# 6 "parser.mly"
       (int)
# 25 "parser.ml"
  )
    | IN
    | IF
    | ID of (
# 8 "parser.mly"
       (string)
# 32 "parser.ml"
  )
    | FUN
    | EQ
    | ELSE
    | DIV
    | CAMMA
    | BOOL of (
# 7 "parser.mly"
       (bool)
# 42 "parser.ml"
  )
    | ARROW
  
end

include MenhirBasics

# 1 "parser.mly"
  
  open Syntax
  (* ここに書いたものは，parser.mliに入らないので注意 *)

# 55 "parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_toplevel) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: toplevel. *)

  | MenhirState01 : (('s, _menhir_box_toplevel) _menhir_cell1_MINUS, _menhir_box_toplevel) _menhir_state
    (** State 01.
        Stack shape : MINUS.
        Start symbol: toplevel. *)

  | MenhirState02 : (('s, _menhir_box_toplevel) _menhir_cell1_LPAR, _menhir_box_toplevel) _menhir_state
    (** State 02.
        Stack shape : LPAR.
        Start symbol: toplevel. *)

  | MenhirState03 : (('s, _menhir_box_toplevel) _menhir_cell1_LPAR, _menhir_box_toplevel) _menhir_state
    (** State 03.
        Stack shape : LPAR.
        Start symbol: toplevel. *)

  | MenhirState04 : (('s, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_state
    (** State 04.
        Stack shape : LET.
        Start symbol: toplevel. *)

  | MenhirState05 : ((('s, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_state
    (** State 05.
        Stack shape : LET REC.
        Start symbol: toplevel. *)

  | MenhirState07 : (((('s, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_state
    (** State 07.
        Stack shape : LET REC var.
        Start symbol: toplevel. *)

  | MenhirState09 : ((((('s, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_state
    (** State 09.
        Stack shape : LET REC var var.
        Start symbol: toplevel. *)

  | MenhirState11 : (('s, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_state
    (** State 11.
        Stack shape : IF.
        Start symbol: toplevel. *)

  | MenhirState13 : (('s, _menhir_box_toplevel) _menhir_cell1_FUN, _menhir_box_toplevel) _menhir_state
    (** State 13.
        Stack shape : FUN.
        Start symbol: toplevel. *)

  | MenhirState15 : ((('s, _menhir_box_toplevel) _menhir_cell1_FUN, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_state
    (** State 15.
        Stack shape : FUN var.
        Start symbol: toplevel. *)

  | MenhirState19 : (('s, _menhir_box_toplevel) _menhir_cell1_factor_expr, _menhir_box_toplevel) _menhir_state
    (** State 19.
        Stack shape : factor_expr.
        Start symbol: toplevel. *)

  | MenhirState22 : (('s, _menhir_box_toplevel) _menhir_cell1_app_expr, _menhir_box_toplevel) _menhir_state
    (** State 22.
        Stack shape : app_expr.
        Start symbol: toplevel. *)

  | MenhirState24 : (('s, _menhir_box_toplevel) _menhir_cell1_factor_expr, _menhir_box_toplevel) _menhir_state
    (** State 24.
        Stack shape : factor_expr.
        Start symbol: toplevel. *)

  | MenhirState28 : (('s, _menhir_box_toplevel) _menhir_cell1_arith_expr, _menhir_box_toplevel) _menhir_state
    (** State 28.
        Stack shape : arith_expr.
        Start symbol: toplevel. *)

  | MenhirState30 : (('s, _menhir_box_toplevel) _menhir_cell1_arith_expr, _menhir_box_toplevel) _menhir_state
    (** State 30.
        Stack shape : arith_expr.
        Start symbol: toplevel. *)

  | MenhirState32 : (('s, _menhir_box_toplevel) _menhir_cell1_arith_expr, _menhir_box_toplevel) _menhir_state
    (** State 32.
        Stack shape : arith_expr.
        Start symbol: toplevel. *)

  | MenhirState34 : (('s, _menhir_box_toplevel) _menhir_cell1_arith_expr, _menhir_box_toplevel) _menhir_state
    (** State 34.
        Stack shape : arith_expr.
        Start symbol: toplevel. *)

  | MenhirState37 : ((('s, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_state
    (** State 37.
        Stack shape : IF expr.
        Start symbol: toplevel. *)

  | MenhirState39 : (((('s, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_state
    (** State 39.
        Stack shape : IF expr expr.
        Start symbol: toplevel. *)

  | MenhirState42 : (((((('s, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_state
    (** State 42.
        Stack shape : LET REC var var expr.
        Start symbol: toplevel. *)

  | MenhirState45 : ((('s, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_state
    (** State 45.
        Stack shape : LET var.
        Start symbol: toplevel. *)

  | MenhirState47 : (((('s, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_state
    (** State 47.
        Stack shape : LET var expr.
        Start symbol: toplevel. *)

  | MenhirState51 : ((('s, _menhir_box_toplevel) _menhir_cell1_LPAR, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_state
    (** State 51.
        Stack shape : LPAR expr.
        Start symbol: toplevel. *)

  | MenhirState56 : (('s, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_state
    (** State 56.
        Stack shape : LET.
        Start symbol: toplevel. *)

  | MenhirState57 : ((('s, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_state
    (** State 57.
        Stack shape : LET REC.
        Start symbol: toplevel. *)

  | MenhirState58 : (((('s, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_state
    (** State 58.
        Stack shape : LET REC var.
        Start symbol: toplevel. *)

  | MenhirState60 : ((((('s, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_state
    (** State 60.
        Stack shape : LET REC var var.
        Start symbol: toplevel. *)

  | MenhirState64 : ((('s, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_state
    (** State 64.
        Stack shape : LET var.
        Start symbol: toplevel. *)


and ('s, 'r) _menhir_cell1_app_expr = 
  | MenhirCell1_app_expr of 's * ('s, 'r) _menhir_state * (Syntax.expr)

and ('s, 'r) _menhir_cell1_arith_expr = 
  | MenhirCell1_arith_expr of 's * ('s, 'r) _menhir_state * (Syntax.expr)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Syntax.expr)

and ('s, 'r) _menhir_cell1_factor_expr = 
  | MenhirCell1_factor_expr of 's * ('s, 'r) _menhir_state * (Syntax.expr)

and ('s, 'r) _menhir_cell1_var = 
  | MenhirCell1_var of 's * ('s, 'r) _menhir_state * (string)

and ('s, 'r) _menhir_cell1_FUN = 
  | MenhirCell1_FUN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAR = 
  | MenhirCell1_LPAR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REC = 
  | MenhirCell1_REC of 's * ('s, 'r) _menhir_state

and _menhir_box_toplevel = 
  | MenhirBox_toplevel of (Syntax.command) [@@unboxed]

let _menhir_action_01 =
  fun _1 _2 ->
    (
# 57 "parser.mly"
                         ( EApp(_1, _2) )
# 245 "parser.ml"
     : (Syntax.expr))

let _menhir_action_02 =
  fun _1 ->
    (
# 58 "parser.mly"
                         ( _1 )
# 253 "parser.ml"
     : (Syntax.expr))

let _menhir_action_03 =
  fun _1 _3 ->
    (
# 40 "parser.mly"
                                 ( EAdd(_1,_3) )
# 261 "parser.ml"
     : (Syntax.expr))

let _menhir_action_04 =
  fun _1 _3 ->
    (
# 41 "parser.mly"
                                 ( ESub(_1,_3) )
# 269 "parser.ml"
     : (Syntax.expr))

let _menhir_action_05 =
  fun _1 ->
    (
# 42 "parser.mly"
                                 ( _1 )
# 277 "parser.ml"
     : (Syntax.expr))

let _menhir_action_06 =
  fun _1 ->
    (
# 62 "parser.mly"
                   ( EConstInt(_1) )
# 285 "parser.ml"
     : (Syntax.expr))

let _menhir_action_07 =
  fun _1 ->
    (
# 63 "parser.mly"
                   ( EConstBool(_1) )
# 293 "parser.ml"
     : (Syntax.expr))

let _menhir_action_08 =
  fun _1 ->
    (
# 64 "parser.mly"
                   ( EVar(_1) )
# 301 "parser.ml"
     : (Syntax.expr))

let _menhir_action_09 =
  fun _2 ->
    (
# 65 "parser.mly"
                   ( _2 )
# 309 "parser.ml"
     : (Syntax.expr))

let _menhir_action_10 =
  fun _2 _4 _6 ->
    (
# 29 "parser.mly"
                                    ( ELet(_2,_4,_6) )
# 317 "parser.ml"
     : (Syntax.expr))

let _menhir_action_11 =
  fun _3 _4 _6 _8 ->
    (
# 30 "parser.mly"
                                    ( ELetRec(_3,_4,_6,_8) )
# 325 "parser.ml"
     : (Syntax.expr))

let _menhir_action_12 =
  fun _2 _4 _6 ->
    (
# 31 "parser.mly"
                                    ( EIf(_2,_4,_6) )
# 333 "parser.ml"
     : (Syntax.expr))

let _menhir_action_13 =
  fun _2 _4 ->
    (
# 32 "parser.mly"
                                    ( EFun(_2,_4) )
# 341 "parser.ml"
     : (Syntax.expr))

let _menhir_action_14 =
  fun _2 _4 ->
    (
# 33 "parser.mly"
                                    ( EPair(_2,_4) )
# 349 "parser.ml"
     : (Syntax.expr))

let _menhir_action_15 =
  fun _1 _3 ->
    (
# 34 "parser.mly"
                                    ( EEq(_1,_3) )
# 357 "parser.ml"
     : (Syntax.expr))

let _menhir_action_16 =
  fun _1 _3 ->
    (
# 35 "parser.mly"
                                    ( ELt(_1,_3) )
# 365 "parser.ml"
     : (Syntax.expr))

let _menhir_action_17 =
  fun _1 ->
    (
# 36 "parser.mly"
                                    ( _1 )
# 373 "parser.ml"
     : (Syntax.expr))

let _menhir_action_18 =
  fun _1 _3 ->
    (
# 46 "parser.mly"
                                ( EMul(_1,_3) )
# 381 "parser.ml"
     : (Syntax.expr))

let _menhir_action_19 =
  fun _1 _3 ->
    (
# 47 "parser.mly"
                                ( EDiv(_1,_3) )
# 389 "parser.ml"
     : (Syntax.expr))

let _menhir_action_20 =
  fun _1 ->
    (
# 48 "parser.mly"
                                ( _1 )
# 397 "parser.ml"
     : (Syntax.expr))

let _menhir_action_21 =
  fun _2 ->
    (
# 52 "parser.mly"
                    ( ESub(EConstInt(0),_2) )
# 405 "parser.ml"
     : (Syntax.expr))

let _menhir_action_22 =
  fun _1 ->
    (
# 53 "parser.mly"
            ( _1 )
# 413 "parser.ml"
     : (Syntax.expr))

let _menhir_action_23 =
  fun _1 ->
    (
# 23 "parser.mly"
                                     ( CExp _1 )
# 421 "parser.ml"
     : (Syntax.command))

let _menhir_action_24 =
  fun _2 _4 ->
    (
# 24 "parser.mly"
                                     ( CDecl (_2, _4) )
# 429 "parser.ml"
     : (Syntax.command))

let _menhir_action_25 =
  fun _3 _4 _6 ->
    (
# 25 "parser.mly"
                                     ( CRecDecl (_3,_4,_6) )
# 437 "parser.ml"
     : (Syntax.command))

let _menhir_action_26 =
  fun _1 ->
    (
# 69 "parser.mly"
       ( _1 )
# 445 "parser.ml"
     : (string))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ARROW ->
        "ARROW"
    | BOOL _ ->
        "BOOL"
    | CAMMA ->
        "CAMMA"
    | DIV ->
        "DIV"
    | ELSE ->
        "ELSE"
    | EQ ->
        "EQ"
    | FUN ->
        "FUN"
    | ID _ ->
        "ID"
    | IF ->
        "IF"
    | IN ->
        "IN"
    | INT _ ->
        "INT"
    | LET ->
        "LET"
    | LPAR ->
        "LPAR"
    | LT ->
        "LT"
    | MINUS ->
        "MINUS"
    | PLUS ->
        "PLUS"
    | REC ->
        "REC"
    | RPAR ->
        "RPAR"
    | SEMISEMI ->
        "SEMISEMI"
    | THEN ->
        "THEN"
    | TIMES ->
        "TIMES"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_goto_toplevel : type  ttv_stack. ttv_stack -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _v ->
      MenhirBox_toplevel _v
  
  let rec _menhir_run_68 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMISEMI ->
          let _1 = _v in
          let _v = _menhir_action_23 _1 in
          _menhir_goto_toplevel _menhir_stack _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | MINUS ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | LPAR ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_06 _1 in
          _menhir_run_21_spec_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          _menhir_run_21_spec_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_07 _1 in
          _menhir_run_21_spec_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | MINUS ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | LPAR ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | LET ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_06 _1 in
          _menhir_run_21_spec_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          _menhir_run_21_spec_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FUN ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState02
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_07 _1 in
          _menhir_run_21_spec_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | MINUS ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | LPAR ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | LET ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_06 _1 in
          _menhir_run_21_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          _menhir_run_21_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FUN ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_07 _1 in
          _menhir_run_21_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REC ->
          let _menhir_stack = MenhirCell1_REC (_menhir_stack, MenhirState04) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v =
                let _1 = _v in
                _menhir_action_26 _1
              in
              let _menhir_stack = MenhirCell1_var (_menhir_stack, MenhirState05, _v) in
              (match (_tok : MenhirBasics.token) with
              | ID _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v_1 =
                    let _1 = _v_0 in
                    _menhir_action_26 _1
                  in
                  let _menhir_stack = MenhirCell1_var (_menhir_stack, MenhirState07, _v_1) in
                  (match (_tok : MenhirBasics.token) with
                  | EQ ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | MINUS ->
                          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
                      | LPAR ->
                          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
                      | LET ->
                          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
                      | INT _v_2 ->
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          let _1 = _v_2 in
                          let _v = _menhir_action_06 _1 in
                          _menhir_run_21_spec_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                      | IF ->
                          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
                      | ID _v_4 ->
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          let _1 = _v_4 in
                          let _v = _menhir_action_08 _1 in
                          _menhir_run_21_spec_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                      | FUN ->
                          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
                      | BOOL _v_6 ->
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          let _1 = _v_6 in
                          let _v = _menhir_action_07 _1 in
                          _menhir_run_21_spec_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let _1 = _v in
            _menhir_action_26 _1
          in
          let _menhir_stack = MenhirCell1_var (_menhir_stack, MenhirState04, _v) in
          (match (_tok : MenhirBasics.token) with
          | EQ ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | MINUS ->
                  _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState45
              | LPAR ->
                  _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState45
              | LET ->
                  _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState45
              | INT _v_8 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _1 = _v_8 in
                  let _v = _menhir_action_06 _1 in
                  _menhir_run_21_spec_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | IF ->
                  _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState45
              | ID _v_10 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _1 = _v_10 in
                  let _v = _menhir_action_08 _1 in
                  _menhir_run_21_spec_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | FUN ->
                  _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState45
              | BOOL _v_12 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _1 = _v_12 in
                  let _v = _menhir_action_07 _1 in
                  _menhir_run_21_spec_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_21_spec_09 : type  ttv_stack. ((((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState09 _tok
  
  and _menhir_run_22 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState22
      | INT _v_0 ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_0 in
          let _v = _menhir_action_06 _1 in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v_2 ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_2 in
          let _v = _menhir_action_08 _1 in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BOOL _v_4 ->
          let _menhir_stack = MenhirCell1_app_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_4 in
          let _v = _menhir_action_07 _1 in
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | CAMMA | DIV | ELSE | EQ | IN | LT | MINUS | PLUS | RPAR | SEMISEMI | THEN | TIMES ->
          let _1 = _v in
          let _v = _menhir_action_22 _1 in
          _menhir_goto_minus_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_23 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_app_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_app_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_01 _1 _2 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_minus_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState01 ->
          _menhir_run_55 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState24 ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState19 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_17_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState64 ->
          _menhir_run_17_spec_64 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState60 ->
          _menhir_run_17_spec_60 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState02 ->
          _menhir_run_17_spec_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState51 ->
          _menhir_run_17_spec_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState03 ->
          _menhir_run_17_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState47 ->
          _menhir_run_17_spec_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState45 ->
          _menhir_run_17_spec_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState42 ->
          _menhir_run_17_spec_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState09 ->
          _menhir_run_17_spec_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState39 ->
          _menhir_run_17_spec_39 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState37 ->
          _menhir_run_17_spec_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState11 ->
          _menhir_run_17_spec_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState34 ->
          _menhir_run_17_spec_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState32 ->
          _menhir_run_17_spec_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState30 ->
          _menhir_run_17_spec_30 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState28 ->
          _menhir_run_17_spec_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState15 ->
          _menhir_run_17_spec_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_55 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_MINUS (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_21 _2 in
      _menhir_goto_minus_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_25 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_factor_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_factor_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_19 _1 _3 in
      _menhir_goto_factor_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_factor_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState30 ->
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState28 ->
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState64 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState60 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState51 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState47 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState45 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState42 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState39 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState37 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState34 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState32 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState15 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_31 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_factor_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_factor_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CAMMA | ELSE | EQ | IN | LT | MINUS | PLUS | RPAR | SEMISEMI | THEN ->
          let MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_04 _1 _3 in
          _menhir_goto_arith_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_19 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_factor_expr -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | MINUS ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | LPAR ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_06 _1 in
          _menhir_run_21_spec_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          _menhir_run_21_spec_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_07 _1 in
          _menhir_run_21_spec_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_21_spec_19 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_factor_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState19 _tok
  
  and _menhir_run_24 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_factor_expr -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | MINUS ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState24
      | LPAR ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState24
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_06 _1 in
          _menhir_run_21_spec_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          _menhir_run_21_spec_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_07 _1 in
          _menhir_run_21_spec_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_21_spec_24 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_factor_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState24 _tok
  
  and _menhir_goto_arith_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState34 ->
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState32 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState64 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState60 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState51 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState45 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState47 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState42 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState37 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState39 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState15 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_35 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CAMMA | ELSE | IN | RPAR | SEMISEMI | THEN ->
          let MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_15 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_28 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | MINUS ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | LPAR ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState28
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_06 _1 in
          _menhir_run_21_spec_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          _menhir_run_21_spec_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_07 _1 in
          _menhir_run_21_spec_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_21_spec_28 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState28 _tok
  
  and _menhir_run_30 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | MINUS ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | LPAR ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState30
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_06 _1 in
          _menhir_run_21_spec_30 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          _menhir_run_21_spec_30 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_07 _1 in
          _menhir_run_21_spec_30 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_21_spec_30 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState30 _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_68 _menhir_stack _v _tok
      | MenhirState64 ->
          _menhir_run_65 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState60 ->
          _menhir_run_61 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState51 ->
          _menhir_run_52 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState03 ->
          _menhir_run_49 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState47 ->
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState45 ->
          _menhir_run_46 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState42 ->
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState09 ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState39 ->
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState37 ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState15 ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_65 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMISEMI ->
          let MenhirCell1_var (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_24 _2 _4 in
          _menhir_goto_toplevel _menhir_stack _v
      | IN ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_47 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | MINUS ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState47
      | LPAR ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState47
      | LET ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState47
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_06 _1 in
          _menhir_run_21_spec_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState47
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          _menhir_run_21_spec_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FUN ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState47
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_07 _1 in
          _menhir_run_21_spec_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_21_spec_47 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState47 _tok
  
  and _menhir_run_11 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | MINUS ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState11
      | LPAR ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState11
      | LET ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState11
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_06 _1 in
          _menhir_run_21_spec_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState11
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          _menhir_run_21_spec_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FUN ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState11
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_07 _1 in
          _menhir_run_21_spec_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_21_spec_11 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState11 _tok
  
  and _menhir_run_13 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let _1 = _v in
            _menhir_action_26 _1
          in
          let _menhir_stack = MenhirCell1_var (_menhir_stack, MenhirState13, _v) in
          (match (_tok : MenhirBasics.token) with
          | ARROW ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | MINUS ->
                  _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
              | LPAR ->
                  _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
              | LET ->
                  _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
              | INT _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _1 = _v_0 in
                  let _v = _menhir_action_06 _1 in
                  _menhir_run_21_spec_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | IF ->
                  _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
              | ID _v_2 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _1 = _v_2 in
                  let _v = _menhir_action_08 _1 in
                  _menhir_run_21_spec_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | FUN ->
                  _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
              | BOOL _v_4 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _1 = _v_4 in
                  let _v = _menhir_action_07 _1 in
                  _menhir_run_21_spec_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_21_spec_15 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_FUN, _menhir_box_toplevel) _menhir_cell1_var -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState15 _tok
  
  and _menhir_run_61 : type  ttv_stack. (((((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMISEMI ->
          let MenhirCell1_var (_menhir_stack, _, _4) = _menhir_stack in
          let MenhirCell1_var (_menhir_stack, _, _3) = _menhir_stack in
          let MenhirCell1_REC (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _) = _menhir_stack in
          let _6 = _v in
          let _v = _menhir_action_25 _3 _4 _6 in
          _menhir_goto_toplevel _menhir_stack _v
      | IN ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_42 : type  ttv_stack. (((((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | MINUS ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState42
      | LPAR ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState42
      | LET ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState42
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_06 _1 in
          _menhir_run_21_spec_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState42
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          _menhir_run_21_spec_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FUN ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState42
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_07 _1 in
          _menhir_run_21_spec_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_21_spec_42 : type  ttv_stack. (((((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState42 _tok
  
  and _menhir_run_54 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_50 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_LPAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_09 _2 in
      _menhir_goto_atomic_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_atomic_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState22 ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_21_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState64 ->
          _menhir_run_21_spec_64 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState60 ->
          _menhir_run_21_spec_60 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState01 ->
          _menhir_run_21_spec_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState02 ->
          _menhir_run_21_spec_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState03 ->
          _menhir_run_21_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState51 ->
          _menhir_run_21_spec_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState45 ->
          _menhir_run_21_spec_45 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState47 ->
          _menhir_run_21_spec_47 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState09 ->
          _menhir_run_21_spec_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState42 ->
          _menhir_run_21_spec_42 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState11 ->
          _menhir_run_21_spec_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState37 ->
          _menhir_run_21_spec_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState39 ->
          _menhir_run_21_spec_39 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState34 ->
          _menhir_run_21_spec_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState32 ->
          _menhir_run_21_spec_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState30 ->
          _menhir_run_21_spec_30 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState28 ->
          _menhir_run_21_spec_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState15 ->
          _menhir_run_21_spec_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState24 ->
          _menhir_run_21_spec_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState19 ->
          _menhir_run_21_spec_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_21_spec_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
  
  and _menhir_run_21_spec_64 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState64 _tok
  
  and _menhir_run_21_spec_60 : type  ttv_stack. ((((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState60 _tok
  
  and _menhir_run_21_spec_01 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
  
  and _menhir_run_21_spec_02 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState02 _tok
  
  and _menhir_run_21_spec_03 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
  
  and _menhir_run_21_spec_51 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState51 _tok
  
  and _menhir_run_21_spec_45 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState45 _tok
  
  and _menhir_run_21_spec_37 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState37 _tok
  
  and _menhir_run_21_spec_39 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState39 _tok
  
  and _menhir_run_21_spec_34 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState34 _tok
  
  and _menhir_run_21_spec_32 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState32 _tok
  
  and _menhir_run_52 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_LPAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_14 _2 _4 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_49 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CAMMA ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | MINUS ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState51
          | LPAR ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState51
          | LET ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState51
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_0 in
              let _v = _menhir_action_06 _1 in
              _menhir_run_21_spec_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | IF ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState51
          | ID _v_2 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_2 in
              let _v = _menhir_action_08 _1 in
              _menhir_run_21_spec_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | FUN ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState51
          | BOOL _v_4 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_4 in
              let _v = _menhir_action_07 _1 in
              _menhir_run_21_spec_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_48 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_var (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_10 _2 _4 _6 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_46 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          _menhir_run_47 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_43 : type  ttv_stack. (((((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _6) = _menhir_stack in
      let MenhirCell1_var (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_var (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_REC (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
      let _8 = _v in
      let _v = _menhir_action_11 _3 _4 _6 _8 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_41 : type  ttv_stack. (((((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_40 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_12 _2 _4 _6 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_38 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | MINUS ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState39
          | LPAR ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState39
          | LET ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState39
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_0 in
              let _v = _menhir_action_06 _1 in
              _menhir_run_21_spec_39 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | IF ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState39
          | ID _v_2 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_2 in
              let _v = _menhir_action_08 _1 in
              _menhir_run_21_spec_39 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | FUN ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState39
          | BOOL _v_4 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_4 in
              let _v = _menhir_action_07 _1 in
              _menhir_run_21_spec_39 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_36 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | MINUS ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | LPAR ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | LET ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_0 in
              let _v = _menhir_action_06 _1 in
              _menhir_run_21_spec_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | IF ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | ID _v_2 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_2 in
              let _v = _menhir_action_08 _1 in
              _menhir_run_21_spec_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | FUN ->
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | BOOL _v_4 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_4 in
              let _v = _menhir_action_07 _1 in
              _menhir_run_21_spec_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_26 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_FUN, _menhir_box_toplevel) _menhir_cell1_var -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_var (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_FUN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_13 _2 _4 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_33 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CAMMA | ELSE | IN | RPAR | SEMISEMI | THEN ->
          let MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_16 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_27 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | MINUS ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
          | LPAR ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState32
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_0 in
              let _v = _menhir_action_06 _1 in
              _menhir_run_21_spec_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_2 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_2 in
              let _v = _menhir_action_08 _1 in
              _menhir_run_21_spec_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BOOL _v_4 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_4 in
              let _v = _menhir_action_07 _1 in
              _menhir_run_21_spec_32 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | EQ ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | MINUS ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
          | LPAR ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState34
          | INT _v_6 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_6 in
              let _v = _menhir_action_06 _1 in
              _menhir_run_21_spec_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_8 in
              let _v = _menhir_action_08 _1 in
              _menhir_run_21_spec_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BOOL _v_10 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_10 in
              let _v = _menhir_action_07 _1 in
              _menhir_run_21_spec_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | CAMMA | ELSE | IN | RPAR | SEMISEMI | THEN ->
          let _1 = _v in
          let _v = _menhir_action_17 _1 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_29 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_factor_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_factor_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CAMMA | ELSE | EQ | IN | LT | MINUS | PLUS | RPAR | SEMISEMI | THEN ->
          let MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_03 _1 _3 in
          _menhir_goto_arith_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_18 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_factor_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_factor_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CAMMA | ELSE | EQ | IN | LT | MINUS | PLUS | RPAR | SEMISEMI | THEN ->
          let _1 = _v in
          let _v = _menhir_action_05 _1 in
          _menhir_goto_arith_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_20 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_factor_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_factor_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_18 _1 _3 in
      _menhir_goto_factor_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_17_spec_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
  
  and _menhir_run_17_spec_64 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState64 _tok
  
  and _menhir_run_17_spec_60 : type  ttv_stack. ((((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState60 _tok
  
  and _menhir_run_17_spec_02 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState02 _tok
  
  and _menhir_run_17_spec_51 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState51 _tok
  
  and _menhir_run_17_spec_03 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
  
  and _menhir_run_17_spec_47 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState47 _tok
  
  and _menhir_run_17_spec_45 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_var -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState45 _tok
  
  and _menhir_run_17_spec_42 : type  ttv_stack. (((((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState42 _tok
  
  and _menhir_run_17_spec_09 : type  ttv_stack. ((((ttv_stack, _menhir_box_toplevel) _menhir_cell1_LET, _menhir_box_toplevel) _menhir_cell1_REC, _menhir_box_toplevel) _menhir_cell1_var, _menhir_box_toplevel) _menhir_cell1_var -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState09 _tok
  
  and _menhir_run_17_spec_39 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState39 _tok
  
  and _menhir_run_17_spec_37 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState37 _tok
  
  and _menhir_run_17_spec_11 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState11 _tok
  
  and _menhir_run_17_spec_34 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState34 _tok
  
  and _menhir_run_17_spec_32 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState32 _tok
  
  and _menhir_run_17_spec_30 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState30 _tok
  
  and _menhir_run_17_spec_28 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState28 _tok
  
  and _menhir_run_17_spec_15 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_FUN, _menhir_box_toplevel) _menhir_cell1_var -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_20 _1 in
      _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState15 _tok
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | MINUS ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LPAR ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LET ->
          let _menhir_stack = MenhirCell1_LET (_menhir_stack, MenhirState00) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | REC ->
              let _menhir_stack = MenhirCell1_REC (_menhir_stack, MenhirState56) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | ID _v ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v =
                    let _1 = _v in
                    _menhir_action_26 _1
                  in
                  let _menhir_stack = MenhirCell1_var (_menhir_stack, MenhirState57, _v) in
                  (match (_tok : MenhirBasics.token) with
                  | ID _v_0 ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _v_1 =
                        let _1 = _v_0 in
                        _menhir_action_26 _1
                      in
                      let _menhir_stack = MenhirCell1_var (_menhir_stack, MenhirState58, _v_1) in
                      (match (_tok : MenhirBasics.token) with
                      | EQ ->
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          (match (_tok : MenhirBasics.token) with
                          | MINUS ->
                              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState60
                          | LPAR ->
                              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState60
                          | LET ->
                              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState60
                          | INT _v_2 ->
                              let _tok = _menhir_lexer _menhir_lexbuf in
                              let _1 = _v_2 in
                              let _v = _menhir_action_06 _1 in
                              _menhir_run_21_spec_60 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                          | IF ->
                              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState60
                          | ID _v_4 ->
                              let _tok = _menhir_lexer _menhir_lexbuf in
                              let _1 = _v_4 in
                              let _v = _menhir_action_08 _1 in
                              _menhir_run_21_spec_60 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                          | FUN ->
                              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState60
                          | BOOL _v_6 ->
                              let _tok = _menhir_lexer _menhir_lexbuf in
                              let _1 = _v_6 in
                              let _v = _menhir_action_07 _1 in
                              _menhir_run_21_spec_60 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                          | _ ->
                              _eRR ())
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | ID _v ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v =
                let _1 = _v in
                _menhir_action_26 _1
              in
              let _menhir_stack = MenhirCell1_var (_menhir_stack, MenhirState56, _v) in
              (match (_tok : MenhirBasics.token) with
              | EQ ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | MINUS ->
                      _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState64
                  | LPAR ->
                      _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState64
                  | LET ->
                      _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState64
                  | INT _v_8 ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _1 = _v_8 in
                      let _v = _menhir_action_06 _1 in
                      _menhir_run_21_spec_64 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                  | IF ->
                      _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState64
                  | ID _v_10 ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _1 = _v_10 in
                      let _v = _menhir_action_08 _1 in
                      _menhir_run_21_spec_64 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                  | FUN ->
                      _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState64
                  | BOOL _v_12 ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _1 = _v_12 in
                      let _v = _menhir_action_07 _1 in
                      _menhir_run_21_spec_64 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_06 _1 in
          _menhir_run_21_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | IF ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          _menhir_run_21_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FUN ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_07 _1 in
          _menhir_run_21_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
end

let toplevel =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_toplevel v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
