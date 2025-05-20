%{
    open Paux
    open Syntax
    open Prop
         %}

(* tokens *)
(* keywords *)
%token EOF TYPE CONST MACHINE EVENT LET FUN ALL GOTO DO
%token STATE HOT COLD START PLAIN ENTRY EXIT LISTEN ON LOCAL THIS HALT NULL RANDOMBOOL RETURN PROP SYN WITH PARAM ENUM
(* arithmetic operators *)
%token PLUS MINUS STAR DIV LT GT LE GE NEQ EQ NEG
(* logic operators *)
%token NOT AND OR TRUE FALSE IMPL IFF FORALL EXISTS PI IN
(* splitter *)
%token COLON ARROW COMMA BAR SEMICOLON COLONEQ ASSIGN
(* paranthesis *)
%token LSEQPRAN RSEQPRAN LPAR RPAR LEPAR REPAR LBRACKET RBRACKET
(* regex *)
%token DOT EMP EPSILON CTX REPEAT CONCAT
(* type *)
%token INT BOOL SUBTYPING UNIT PRIME
%token <string> IDENT
%token <string> STRING
%token <int> NUMBER

(* start symbol *)
%start <Paux.term> prog_eof
%on_error_reduce item_list
%%

nt:
  | INT {Nt.int_ty}
  | MACHINE {p_machine_ty}
  | BOOL {Nt.bool_ty}
  | UNIT {Nt.unit_ty}
  | n=NUMBER {Nt.Ty_var (Printf.sprintf "x%i" n)}
  | nt1=nt ARROW nt2=nt {Nt.mk_arr nt1 nt2}
  | nt=nt id=IDENT {Nt.Ty_constructor (id, [nt]) }
  | id=IDENT {Nt.mk_uninterp id}
  | LPAR fds=type_fields RPAR {Nt.mk_record None fds}
  | LPAR tuple=type_list RPAR {Nt.Ty_tuple tuple}
  | FORALL n=NUMBER DOT nt=nt {Nt.Ty_poly (Printf.sprintf "x%i" n, nt) }
;

type_fields:
  | id=IDENT COLON nt=nt {[(id #: nt)]}
  | id=IDENT COLON nt=nt COMMA ts=type_fields {(id #: nt) :: ts}
;

type_list:
  | nt=nt {[nt]}
  | nt=nt COMMA ts=type_list {nt :: ts}
  ;

biop:
  | PLUS {"+"}
  | MINUS {"-"}
  | STAR {"*"}
  | DIV {"/"}
  | LT {"<"}
  | GT {">"}
  | LE {"<="}
  | GE {">="}
  | EQ {"=="}
  | NEQ {"!="}
  | IN {"in"}
;

constant:
  | TRUE {B true}
  | FALSE {B false}
  | n=NUMBER {I n}
;

id_eq_expr_list:
| id=IDENT ASSIGN expr=expr COMMA cs=id_eq_expr_list {(id, expr)::cs}
| id=IDENT ASSIGN expr=expr {[(id, expr)]}
;

args:
| c=expr {[c]}
| c=expr COMMA cs=args {c :: cs}

expr:
| RANDOMBOOL {mk (mk_boolgen_lit $startpos) $startpos}
| HALT {mk (halt $startpos) $startpos}
| NULL {mk (null $startpos) $startpos}
| THIS {mk (this $startpos) $startpos}
| c=constant {mk (AC c) $startpos}
| id=IDENT {mk (AVar (id #: ($startpos, Nt.Ty_unknown))) $startpos}
| LPAR lit=expr COMMA args=args RPAR { mk (ATu (lit :: args)) $startpos}
| LPAR id=IDENT ASSIGN expr=expr COMMA es=id_eq_expr_list RPAR {mk (ARecord ((id, expr) ::es)) $startpos}
| record=expr DOT field=NUMBER { mk (AProj (record, field)) $startpos}
| record=expr DOT field=IDENT { mk (AField (record, field)) $startpos}
| NEG e=expr {mk (mk_not_lit e $startpos) $startpos}
| e1=expr op=biop e2=expr {mk (mk_biop_lit op e1 e2 $startpos) $startpos}
| pfunc=IDENT LPAR RPAR {mk (mk_app_lit pfunc [] $startpos) $startpos}
| pfunc=IDENT LPAR args=args RPAR {mk (mk_app_lit pfunc args $startpos) $startpos}
| LPAR e=expr RPAR {e}
;

prop:
| NOT p1=prop { Not p1 }
| p1=prop IMPL p2=prop { Implies (p1, p2)}
| p1=prop IFF p2=prop { Iff (p1, p2)}
| p1=prop AND p2=prop { And [p1; p2]}
| p1=prop OR p2=prop { Or [p1; p2]}
| FORALL LPAR e=IDENT COLON nt=nt RPAR DOT body=prop { Forall {qv = e#:($startpos, nt); body}}
| EXISTS LPAR e=IDENT COLON nt=nt RPAR DOT body=prop { Exists {qv = e#:($startpos, nt); body}}
| LPAR e=prop RPAR {e}
| lit=expr { Lit lit }
;

gen_num:
| event=IDENT COLON n=expr { [event, n]}
| event=IDENT COLON n=expr COMMA cs=gen_num { (event, n) :: cs }
;

constaints:
| name=IDENT { [name]}
| name=IDENT COMMA cs=constaints { name :: cs }
;

id_eq_dest_expr_list:
| id=IDENT ASSIGN LPAR dest=IDENT COMMA expr=expr RPAR COMMA cs=id_eq_dest_expr_list {(id, dest, expr)::cs}
| id=IDENT ASSIGN LPAR dest=IDENT COMMA expr=expr RPAR {[(id, dest, expr)]}
;

ids:
| c=IDENT COMMA cs=ids {c :: cs}
| c=IDENT {[c]}
;

item:
| ENUM id=IDENT LBRACKET ids=ids RBRACKET { PEnumDecl (id, ids) }
| TYPE id=IDENT ASSIGN nt=nt SEMICOLON { PTopSimplDecl { kind = TopType; tvar = (id #:($startpos, nt)) } }
| EVENT id=IDENT COLON nt=nt SEMICOLON { PTopSimplDecl { kind = TopEvent; tvar = (id #:($startpos, nt)) } }
| PARAM id=IDENT COLON nt=nt SEMICOLON { PTopSimplDecl { kind = TopVar; tvar = (id #:($startpos, nt)) } }
| FUN id=IDENT COLON nt=nt SEMICOLON { PTopSimplDecl { kind = TopVar; tvar = (id #:($startpos, nt)) } }
| FUN LPAR id=biop RPAR COLON nt=nt SEMICOLON { PTopSimplDecl { kind = TopVar; tvar = (id #:($startpos, nt)) } }
| PROP name=IDENT ASSIGN prop=prop SEMICOLON { PGlobalProp {name; prop} }
| PROP name=IDENT ON event=IDENT DO id=IDENT WITH prop=prop SEMICOLON
                                     { PPayload {name; self_event = (id #: event); prop} }
| PROP name=IDENT ON event=IDENT DO id=IDENT ASSIGN body=expr SEMICOLON
                                     { PPayloadGen {name; self_event = (id #: event); body} }
| SYN name=IDENT ON LPAR gen_num=id_eq_dest_expr_list RPAR WITH cnames=constaints SEMICOLON
                                                                   { PSyn {name; gen_num; cnames} }
;

item_list:
  | c=item cs=item_list {c :: cs}
  | c=item {[c]}
  ;

prog_eof:
  | s=item_list ; EOF { s }
;
%%
