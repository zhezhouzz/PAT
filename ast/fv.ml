open ParseTree
open Zutils
open Zdatatype
open AutomataLibrary

let rec fv_value (value_e : value) =
  match value_e with VConst _ -> [] | VVar v -> [ v ]

and typed_fv_value (value_e : (Nt.nt, value) typed) = fv_value value_e.x

and fv_term (term_e : term) =
  match term_e with
  | CVal v -> typed_fv_value v
  | CLetE { lhs; rhs; body } ->
      substract_fvs (typed_fv_term body) lhs @ typed_fv_term rhs
  | CAppOp { op; args } -> List.concat (List.map typed_fv_value args)
  | CObs { op; prop } -> fv_prop prop
  | CGen { op; args } -> List.concat (List.map typed_fv_value args)
  | CUnion es -> List.concat_map typed_fv_term es
  | CAssertP phi -> fv_prop phi
  | CAssume (args, prop) -> fv_prop prop
  | CWhile { body; cond } -> fv_prop cond @ typed_fv_term body

and typed_fv_term (term_e : (Nt.nt, term) typed) = fv_term term_e.x

let fv_rich_srl e = fv_rich_regex fv_sevent e

open Prop

let rec fv_cty (cty_e : cty) =
  match cty_e with
  | { phi; nty; _ } -> substract_fvs (fv_prop phi) [ default_v#:nty ]

and typed_fv_cty (cty_e : (Nt.nt, cty) typed) = fv_cty cty_e.x

let rec fv_pat (pat_e : rich_srl pat) =
  match pat_e with
  | RtyBase cty -> fv_cty cty
  | RtyHAF { history; adding; future } ->
      fv_rich_srl history @ fv_rich_srl adding @ fv_rich_srl future
  | RtyHAParallel { history; adding_se; parallel } ->
      fv_rich_srl history @ fv_sevent adding_se
      @ List.concat_map fv_sevent parallel
  | RtyGArr { arg; argnty; retrty } ->
      List.substract (typed_eq String.equal) (fv_pat retrty) [ arg#:argnty ]
  | RtyArr { arg; argcty; retrty } -> fv_cty argcty @ fv_pat retrty
  | RtyInter (p1, p2) -> fv_pat p1 @ fv_pat p2

and typed_fv_pat (pat_e : (Nt.nt, rich_srl pat) typed) = fv_pat pat_e.x

let fv_syn_goal { qvs; prop } = substract_fvs (fv_rich_srl prop) qvs

let rec fv_item (item_e : 't item) =
  match item_e with
  | PrimDecl { name; nt } -> []
  | MsgNtDecl { name; nt; _ } -> []
  | MsgDecl { name; pat } -> fv_pat pat
  | SynGoal syn_goal -> fv_syn_goal syn_goal

and typed_fv_item (item_e : ('t, 't item) typed) = fv_item item_e.x
