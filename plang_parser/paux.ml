open Ast
open Nt

type term = (Lexing.position * nt) p_item list

let mk (x : 'a) loc = { x; ty = (loc, Ty_unknown) }
let halt loc = AAppOp ("halt"#:(loc, unit_ty), [])
let null loc = AAppOp ("null"#:(loc, unit_ty), [])
let this loc = AAppOp ("this"#:(loc, unit_ty), [])
let mk_boolgen_lit loc = AVar "$"#:(loc, bool_ty)

let mk_not_lit lit loc =
  AAppOp ("not"#:(loc, construct_arr_tp ([ bool_ty ], bool_ty)), [ lit ])

let mk_biop_lit op lit1 lit2 loc = AAppOp (op#:(loc, Ty_unknown), [ lit1; lit2 ])
let mk_app_lit op lits loc = AAppOp (op#:(loc, Ty_unknown), lits)
