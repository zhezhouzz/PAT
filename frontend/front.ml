include Syntax
include Common

(* include To_item *)
include To_term

(* let layout_syn_env { gen_ctx; event_tyctx; recvable_ctx; tyctx; goal } = *)
(*   let str = "" in *)
(*   let str = spf "%s\n    tyctx:\n%s\n" str (layout_ctx Nt.layout tyctx) in *)
(*   let str = *)
(*     spf "%s\n    event_tyctx:\n%s\n" str (layout_ctx Nt.layout event_tyctx) *)
(*   in *)
(*   let str = *)
(*     spf "%s\n    gen_ctx:\n%s\n" str (layout_ctx string_of_bool gen_ctx) *)
(*   in *)
(*   let str = *)
(*     spf "%s\n    recvable_ctx:\n%s\n" str *)
(*       (layout_ctx string_of_bool recvable_ctx) *)
(*   in *)
(*   let str = *)
(*     spf "%s\n    goal:\n%s\n" str *)
(*       (match goal with None -> "none" | Some srl -> layout_syn_goal srl) *)
(*   in *)
(*   str *)
