%{
  open Syntax
  (* ここに書いたものは，parser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL
%token <string> ID
%token LET IN
%token PLUS TIMES MINUS DIV
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR
%token FUN ARROW DFUN
%token REC AND
%token SEMISEMI

%start toplevel
%type <Syntax.command> toplevel
%%

toplevel:
  | expr SEMISEMI                    { CExp $1 }
  | LET var EQ expr SEMISEMI         { CDecl ($2, $4) }
  | LET var vars EQ expr SEMISEMI    { CDecl ($2,EFun($3,$5)) }
  | LET REC let_and_decls SEMISEMI   { CRecDecl ($3) }
;

let_and_decls:
  | var var EQ expr AND let_and_decls { ($1,$2,$4) :: $6 }
  | var var vars EQ expr AND let_and_decls { ($1,$2,EFun($3,$5)) :: $7 }
  | var var EQ expr                   { [($1,$2,$4)] }
  | var var vars EQ expr                   { [($1,$2,EFun($3,$5))] }
;

expr:
  | LET var EQ expr IN expr       { ELet($2,$4,$6) }
  | LET REC let_and_decls IN expr { ELetRec($3,$5) }
  | IF expr THEN expr ELSE expr   { EIf($2,$4,$6) }
  | FUN vars ARROW expr            { EFun($2,$4) }
  | DFUN vars ARROW expr            { EDFun($2,$4) }
  | arith_expr EQ arith_expr      { EEq($1,$3) }
  | arith_expr LT arith_expr      { ELt($1,$3) }
  | arith_expr                    { $1 }
;

arith_expr:
  | arith_expr PLUS factor_expr  { EAdd($1,$3) }
  | arith_expr MINUS factor_expr { ESub($1,$3) }
  | factor_expr                  { $1 }
;

factor_expr:
  | factor_expr TIMES app_expr { EMul($1,$3) }
  | factor_expr DIV app_expr   { EDiv($1,$3) }
  | app_expr                   { $1 }
;

app_expr:
  | app_expr atomic_expr { EApp($1, $2) }
  | atomic_expr          { $1 }

atomic_expr:
  | INT            { EConstInt($1) }
  | BOOL           { EConstBool($1) }
  | ID             { EVar($1) }
  | LPAR expr RPAR { $2 }
;

vars:
  | ID vars {$1::$2}
  | ID {[$1]} 
;

var:
  | ID { $1 }
;
