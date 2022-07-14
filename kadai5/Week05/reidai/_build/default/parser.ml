
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | THEN
    | SEMISEMI
    | RPAR
    | PLUS
    | LT
    | LPAR
    | INT of (
# 6 "parser.mly"
       (int)
# 21 "parser.ml"
  )
    | IF
    | ID of (
# 8 "parser.mly"
       (string)
# 27 "parser.ml"
  )
    | EQ
    | ELSE
    | BOOL of (
# 7 "parser.mly"
       (bool)
# 34 "parser.ml"
  )
  
end

include MenhirBasics

# 1 "parser.mly"
  
  open Syntax
  (* ここに書いたものは，parser.mliに入らないので注意 *)

# 46 "parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_toplevel) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: toplevel. *)

  | MenhirState01 : (('s, _menhir_box_toplevel) _menhir_cell1_LPAR, _menhir_box_toplevel) _menhir_state
    (** State 01.
        Stack shape : LPAR.
        Start symbol: toplevel. *)

  | MenhirState03 : (('s, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_state
    (** State 03.
        Stack shape : IF.
        Start symbol: toplevel. *)

  | MenhirState08 : ((('s, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_state
    (** State 08.
        Stack shape : IF expr.
        Start symbol: toplevel. *)

  | MenhirState10 : (((('s, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_state
    (** State 10.
        Stack shape : IF expr expr.
        Start symbol: toplevel. *)

  | MenhirState14 : (('s, _menhir_box_toplevel) _menhir_cell1_arith_expr, _menhir_box_toplevel) _menhir_state
    (** State 14.
        Stack shape : arith_expr.
        Start symbol: toplevel. *)

  | MenhirState16 : (('s, _menhir_box_toplevel) _menhir_cell1_arith_expr, _menhir_box_toplevel) _menhir_state
    (** State 16.
        Stack shape : arith_expr.
        Start symbol: toplevel. *)

  | MenhirState18 : (('s, _menhir_box_toplevel) _menhir_cell1_arith_expr, _menhir_box_toplevel) _menhir_state
    (** State 18.
        Stack shape : arith_expr.
        Start symbol: toplevel. *)


and ('s, 'r) _menhir_cell1_arith_expr = 
  | MenhirCell1_arith_expr of 's * ('s, 'r) _menhir_state * (Syntax.expr)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Syntax.expr)

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAR = 
  | MenhirCell1_LPAR of 's * ('s, 'r) _menhir_state

and _menhir_box_toplevel = 
  | MenhirBox_toplevel of (Syntax.command) [@@unboxed]

let _menhir_action_01 =
  fun _1 _3 ->
    (
# 31 "parser.mly"
                                ( EAdd(_1,_3) )
# 110 "parser.ml"
     : (Syntax.expr))

let _menhir_action_02 =
  fun _1 ->
    (
# 32 "parser.mly"
                                ( _1 )
# 118 "parser.ml"
     : (Syntax.expr))

let _menhir_action_03 =
  fun _1 ->
    (
# 40 "parser.mly"
                   ( EConstInt(_1) )
# 126 "parser.ml"
     : (Syntax.expr))

let _menhir_action_04 =
  fun _1 ->
    (
# 41 "parser.mly"
                   ( EConstBool(_1) )
# 134 "parser.ml"
     : (Syntax.expr))

let _menhir_action_05 =
  fun _1 ->
    (
# 42 "parser.mly"
                   ( EVar(_1) )
# 142 "parser.ml"
     : (Syntax.expr))

let _menhir_action_06 =
  fun _2 ->
    (
# 43 "parser.mly"
                   ( _2 )
# 150 "parser.ml"
     : (Syntax.expr))

let _menhir_action_07 =
  fun _2 _4 _6 ->
    (
# 24 "parser.mly"
                                ( EIf(_2,_4,_6) )
# 158 "parser.ml"
     : (Syntax.expr))

let _menhir_action_08 =
  fun _1 _3 ->
    (
# 25 "parser.mly"
                                ( EEq(_1,_3) )
# 166 "parser.ml"
     : (Syntax.expr))

let _menhir_action_09 =
  fun _1 _3 ->
    (
# 26 "parser.mly"
                                ( ELt(_1,_3) )
# 174 "parser.ml"
     : (Syntax.expr))

let _menhir_action_10 =
  fun _1 ->
    (
# 27 "parser.mly"
                                ( _1 )
# 182 "parser.ml"
     : (Syntax.expr))

let _menhir_action_11 =
  fun _1 ->
    (
# 36 "parser.mly"
                                ( _1 )
# 190 "parser.ml"
     : (Syntax.expr))

let _menhir_action_12 =
  fun _1 ->
    (
# 20 "parser.mly"
                  ( CExp _1 )
# 198 "parser.ml"
     : (Syntax.command))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | BOOL _ ->
        "BOOL"
    | ELSE ->
        "ELSE"
    | EQ ->
        "EQ"
    | ID _ ->
        "ID"
    | IF ->
        "IF"
    | INT _ ->
        "INT"
    | LPAR ->
        "LPAR"
    | LT ->
        "LT"
    | PLUS ->
        "PLUS"
    | RPAR ->
        "RPAR"
    | SEMISEMI ->
        "SEMISEMI"
    | THEN ->
        "THEN"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_23 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMISEMI ->
          let _1 = _v in
          let _v = _menhir_action_12 _1 in
          MenhirBox_toplevel _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_03 _1 in
          _menhir_run_12_spec_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | IF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_05 _1 in
          _menhir_run_12_spec_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_04 _1 in
          _menhir_run_12_spec_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_12_spec_01 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_11 _1 in
      _menhir_run_06_spec_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_06_spec_01 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
  
  and _menhir_run_13 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAR ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_0 in
              let _v = _menhir_action_03 _1 in
              _menhir_run_12_spec_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_2 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_2 in
              let _v = _menhir_action_05 _1 in
              _menhir_run_12_spec_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BOOL _v_4 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_4 in
              let _v = _menhir_action_04 _1 in
              _menhir_run_12_spec_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | EQ ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAR ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
          | INT _v_6 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_6 in
              let _v = _menhir_action_03 _1 in
              _menhir_run_12_spec_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_8 in
              let _v = _menhir_action_05 _1 in
              _menhir_run_12_spec_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BOOL _v_10 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_10 in
              let _v = _menhir_action_04 _1 in
              _menhir_run_12_spec_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | ELSE | RPAR | SEMISEMI | THEN ->
          let _1 = _v in
          let _v = _menhir_action_10 _1 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState14
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_03 _1 in
          _menhir_run_12_spec_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_05 _1 in
          _menhir_run_12_spec_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_04 _1 in
          _menhir_run_12_spec_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_12_spec_14 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_11 _1 in
      _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_15 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_01 _1 _3 in
      _menhir_goto_arith_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_arith_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState18 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState16 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState08 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_19 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | RPAR | SEMISEMI | THEN ->
          let MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_08 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_23 _menhir_stack _v _tok
      | MenhirState01 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState10 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState08 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_20 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_LPAR -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_06 _2 in
          let _1 = _v in
          let _v = _menhir_action_11 _1 in
          _menhir_goto_factor_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_factor_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState14 ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_06_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState01 ->
          _menhir_run_06_spec_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState18 ->
          _menhir_run_06_spec_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState16 ->
          _menhir_run_06_spec_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState10 ->
          _menhir_run_06_spec_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState08 ->
          _menhir_run_06_spec_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState03 ->
          _menhir_run_06_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_06_spec_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
  
  and _menhir_run_06_spec_18 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState18 _tok
  
  and _menhir_run_06_spec_16 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState16 _tok
  
  and _menhir_run_17 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | RPAR | SEMISEMI | THEN ->
          let MenhirCell1_arith_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_09 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_06_spec_10 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState10 _tok
  
  and _menhir_run_06_spec_08 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState08 _tok
  
  and _menhir_run_06_spec_03 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
  
  and _menhir_run_11 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_07 _2 _4 _6 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_09 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAR ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_0 in
              let _v = _menhir_action_03 _1 in
              _menhir_run_12_spec_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | IF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
          | ID _v_2 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_2 in
              let _v = _menhir_action_05 _1 in
              _menhir_run_12_spec_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BOOL _v_4 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_4 in
              let _v = _menhir_action_04 _1 in
              _menhir_run_12_spec_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_12_spec_10 : type  ttv_stack. (((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_11 _1 in
      _menhir_run_06_spec_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_toplevel) _menhir_state -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_03 _1 in
          _menhir_run_12_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | IF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_05 _1 in
          _menhir_run_12_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_04 _1 in
          _menhir_run_12_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_12_spec_03 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_11 _1 in
      _menhir_run_06_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_07 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_toplevel) _menhir_state -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAR ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState08
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_0 in
              let _v = _menhir_action_03 _1 in
              _menhir_run_12_spec_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | IF ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState08
          | ID _v_2 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_2 in
              let _v = _menhir_action_05 _1 in
              _menhir_run_12_spec_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BOOL _v_4 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_4 in
              let _v = _menhir_action_04 _1 in
              _menhir_run_12_spec_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_12_spec_08 : type  ttv_stack. ((ttv_stack, _menhir_box_toplevel) _menhir_cell1_IF, _menhir_box_toplevel) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_11 _1 in
      _menhir_run_06_spec_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_12_spec_16 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_11 _1 in
      _menhir_run_06_spec_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_12_spec_18 : type  ttv_stack. (ttv_stack, _menhir_box_toplevel) _menhir_cell1_arith_expr -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_11 _1 in
      _menhir_run_06_spec_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  let rec _menhir_run_12_spec_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_11 _1 in
      _menhir_run_06_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_toplevel =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_03 _1 in
          _menhir_run_12_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | IF ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_05 _1 in
          _menhir_run_12_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BOOL _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_04 _1 in
          _menhir_run_12_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
end

let toplevel =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_toplevel v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
