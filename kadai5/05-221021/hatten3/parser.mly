%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL
%token <string> ID
%token LET IN
%token PLUS TIMES MINUS DIV
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR
%token AND OR NOT LETAND
%token SEMISEMI

%start toplevel
%type <Syntax.command> toplevel
%%

toplevel:
  | expr SEMISEMI { CExp $1 }
  | LET var EQ expr  LETAND letand{ CLetAnd ($2, $4,$6) }
  | LET var EQ expr SEMISEMI { CDecl ($2, $4) }
  | LET var EQ expr decl { CDecl1 ($2, $4,$5) }
;

letand:
  |var EQ expr LETAND letand{CLetAnd($1,$3,$5)}
  |var EQ expr SEMISEMI { CDecl ($1, $3) }
  |var EQ expr IN expr SEMISEMI { CLet ($1, $3,$5) }
;

decl:
  |LET var EQ expr decl { CDecl1 ($2, $4,$5) }
  | LET var EQ expr SEMISEMI { CDecl ($2, $4) }
;


expr:
  | LET var EQ expr IN expr     { ELet($2,$4,$6) }
  | IF expr THEN expr ELSE expr { EIf($2,$4,$6) }
  | expr1                  { $1 }
;

expr1:
  |expr2 AND expr1 {EAnd($1,$3)}
  |expr2 OR expr1 {EOr($1,$3)}
  |expr2                  { $1 }
;

expr2:
  | expr2 EQ arith_expr   { EEq($1,$3) }
  | expr2 LT arith_expr    { ELt($1,$3) }
  | arith_expr                 { $1 }
;


arith_expr:
  | arith_expr PLUS factor_expr  { EAdd($1,$3) }
  | arith_expr MINUS factor_expr { ESub($1,$3) }
  | factor_expr                  { $1 }
;

factor_expr:
  | factor_expr TIMES atomic_expr { EMul($1,$3) }
  | factor_expr DIV atomic_expr   { EDiv($1,$3) }
  | expr3                   { $1 }
;

expr3:
  |NOT expr3 {ENot($2)}
  |atomic_expr                   { $1 }
;

atomic_expr:
  | INT            { EConstInt($1) }
  | BOOL           { EConstBool($1) }
  | ID             { EVar($1) }
  | LPAR expr RPAR { $2 }
;

var:
  | ID { $1 }
;
