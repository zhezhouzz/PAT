include Syntax
include Common
(* include To_regex *)

(* include To_inst *)
include To_rty
include To_item
include To_term
(* module Nt = struct *)
(*   include Nt.T *)
(*   include Nt *)
(* end *)

open AutomataLibrary
open Zdatatype

let layout_syn_env
    {
      event_rich_rtyctx;
      event_rtyctx;
      msgkind_ctx;
      event_tyctx;
      tyctx;
      goals;
      axioms;
    } =
  let str = "" in
  let str = spf "%s\n    tyctx:\n%s\n" str (layout_ctx Nt.layout tyctx) in
  let str =
    spf "%s\n    event_tyctx:\n%s\n" str (layout_ctx Nt.layout event_tyctx)
  in
  let str =
    spf "%s\n    msgkind_ctx:\n%s\n" str (layout_ctx layout_msgkind msgkind_ctx)
  in
  let str =
    spf "%s\n    event_rich_rtyctx: %i\n" str
      (List.length @@ ctx_to_list event_rich_rtyctx)
  in
  let str =
    spf "%s\n    event_rtyctx:\n%s\n" str
      (layout_ctx (layout_pat SFA.layout_regex) event_rtyctx)
  in
  let str =
    spf "%s\n    goals:\n%s\n" str
      (List.split_by "\n" (fun (x, goal) ->
           spf "%s: %s" x (layout_syn_goal goal))
      @@ StrMap.to_kv_list goals)
  in
  let str = spf "%s\n    axioms (%i):\n" str (StrMap.cardinal axioms) in
  let str =
    spf "%s\n%s\n" str
      (List.split_by "\n"
         (fun (name, prop) -> spf "%s: %s" name (layout_prop prop))
         (StrMap.to_kv_list axioms))
  in
  str
