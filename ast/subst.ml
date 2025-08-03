open ParseTree
open Zutils
open AutomataLibrary

let rec subst_value (string_x : string) f (value_e : value) =
  match value_e with
  | VConst constant0 -> VConst constant0
  | VVar v ->
      if String.equal v.x string_x then
        match f v with AC c -> VConst c | _ -> _die [%here]
      else VVar v

and typed_subst_value (string_x : string) f (value_e : (Nt.nt, value) typed) =
  value_e#->(subst_value string_x f)

and subst_term (string_x : string) f (term_e : term) =
  match term_e with
  | CVal v -> CVal (typed_subst_value string_x f v)
  | CLetE { lhs; rhs; body } ->
      let rhs = typed_subst_term string_x f rhs in
      if List.exists (fun x -> String.equal x.x string_x) lhs then
        CLetE { rhs; lhs; body }
      else CLetE { rhs; lhs; body = typed_subst_term string_x f body }
  | CAppOp { op; args } ->
      CAppOp { op; args = List.map (typed_subst_value string_x f) args }
  | CObs { op; prop } -> CObs { op; prop = subst_prop string_x f prop }
  | CGen { op; args } ->
      CGen { op; args = List.map (typed_subst_value string_x f) args }
  | CUnion es -> CUnion (List.map (typed_subst_term string_x f) es)
  | CAssertP phi -> CAssertP (subst_prop string_x f phi)
  | CAssume (args, prop) -> CAssume (args, subst_prop string_x f prop)
  | CWhile { body; cond } ->
      CWhile
        {
          body = typed_subst_term string_x f body;
          cond = subst_prop string_x f cond;
        }

and typed_subst_term (string_x : string) f (term_e : (Nt.nt, term) typed) =
  term_e#->(subst_term string_x f)

open Prop

let subst_cty (string_x : string) f (cty_e : cty) =
  match cty_e with { phi; nty; _ } -> { phi; nty }

let rec subst_pat_ subst_srl (string_x : string) f (rty_e : 'a pat) =
  match rty_e with
  | RtyBase cty -> RtyBase (subst_cty string_x f cty)
  | RtyArr { arg; argcty; retrty } ->
      let argcty = subst_cty string_x f argcty in
      if String.equal arg string_x then RtyArr { arg; argcty; retrty }
      else
        RtyArr { arg; argcty; retrty = subst_pat_ subst_srl string_x f retrty }
  | RtyGArr { arg; argnty; retrty } ->
      if String.equal arg string_x then RtyGArr { arg; argnty; retrty }
      else
        RtyGArr { arg; argnty; retrty = subst_pat_ subst_srl string_x f retrty }
  | RtyHAF { history; adding; future } ->
      let history, adding, future =
        map3 (subst_srl string_x f) (history, adding, future)
      in
      RtyHAF { history; adding; future }
  | RtyHAParallel { history; adding_se; parallel } ->
      RtyHAParallel
        {
          history = subst_srl string_x f history;
          adding_se = subst_sevent string_x f adding_se;
          parallel = List.map (subst_sevent string_x f) parallel;
        }
  | RtyInter (p1, p2) ->
      RtyInter
        (subst_pat_ subst_srl string_x f p1, subst_pat_ subst_srl string_x f p2)

let subst_rich_srl_pat = subst_pat_ subst_rich_regex
let subst_srl_pat x f (regex : srl pat) = subst_pat_ SFA.subst_regex x f regex
let subst_cty_instance x y z = subst_f_to_instance subst_cty x y z

let subst_rich_srl_instance s lit (srl : rich_srl) =
  subst_rich_regex_instance s lit srl

let subst_rich_srl_pat_instance y z sevent =
  subst_f_to_instance subst_rich_srl_pat y z sevent

let subst_srl_pat_instance y z sevent =
  subst_f_to_instance subst_srl_pat y z sevent
