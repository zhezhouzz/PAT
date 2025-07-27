open Language
open Zdatatype
open Common
open AutomataLibrary.SFA

module SampleDomain = Map.Make (struct
  type t = Nt.t

  let compare = Nt.compare_nt
end)

type env = {
  event_rtyctx : CharSet.t regex pat ctx;
  event_tyctx : t ctx;
  tyctx : Nt.t ctx;
  gen_ctx : bool ctx;
  recvable_ctx : bool ctx;
  goal : syn_goal option;
}

type runtime = {
  sample_domain : constant list SampleDomain.t;
  trace : trace;
  buffer : trace;
  store : constant StrMap.t;
  event_rtyctx : CharSet.t regex pat ctx;
}

exception RuntimeInconsistent of runtime

let layout_store store =
  List.split_by " ;" (fun (x, c) -> spf "%s --> %s" x (layout_constant c))
  @@ StrMap.to_kv_list store

let layout_runtime { buffer; store; event_rtyctx; _ } =
  let str = "" in
  let str =
    spf "%s\n    buffer:\n%s\n" str
      (List.split_by "\n" layout_trace_elem buffer)
  in
  let str = spf "%s\n    store:\n%s\n" str (layout_store store) in
  let str =
    spf "%s\n    event_rtyctx:\n%s\n" str
      (layout_ctx (layout_pat SFA.layout_regex) event_rtyctx)
  in
  str

let default_sample_domain =
  SampleDomain.of_seq @@ List.to_seq
  @@ [
       (Nt.int_ty, List.map (fun n -> I n) [ -1; 0; 1; 2; 3; 4 ]);
       (Nt.bool_ty, List.map (fun n -> B n) [ true; false ]);
       (mk_p_abstract_ty "rid", List.map (fun n -> I n) [ 1; 2; 3 ]);
       (mk_p_abstract_ty "aid", List.map (fun n -> I n) [ 4; 5; 6 ]);
       (mk_p_abstract_ty "tGid", List.map (fun n -> I n) [ 1; 2; 3 ]);
       (mk_p_abstract_ty "tKey", List.map (fun n -> I n) [ 4; 5; 6 ]);
       (mk_p_abstract_ty "tVal", List.map (fun n -> I n) [ 7; 8; 9 ]);
     ]

let init_runtime (env : syn_env) sample_domain =
  {
    sample_domain;
    trace = [];
    buffer = [];
    store = StrMap.empty;
    event_rtyctx = env.event_rtyctx;
  }

let store_add (vs, cs) store =
  StrMap.add_seq
    (List.to_seq
    @@ List.map (fun (x, c) -> (x.x, c))
    @@ _safe_combine [%here] vs cs)
    store

let reduce_cty c ({ phi; _ } : cty) =
  eval_prop (StrMap.singleton default_v c) phi

let reduce_sevent (op', (cs : constant list)) = function
  | { op; vs; phi } ->
      String.equal op op' && eval_prop (store_add (vs, cs) StrMap.empty) phi

let sample runtime qv =
  match qv.ty with
  (* | Nt.Ty_enum { enum_elems; enum_name } ->
      let elem = choose_from_list enum_elems in
      let c = Enum { enum_elems; enum_name; elem } in
      (qv.x, c) *)
  | _ -> (
      match SampleDomain.find_opt qv.ty runtime.sample_domain with
      | None ->
          let () =
            Printf.printf "cannot find sample domain of type (%s)\n"
              (Nt.layout qv.ty)
          in
          _die [%here]
      | Some cs -> (qv.x, choose_from_list cs))

let reduce_pat runtime cs (tau : CharSet.t regex pat) =
  let rec aux (tau, cs) =
    (* let () = *)
    (*   Pp.printf "@{<bold>reduce_haft:@} cs(%s)\n%s\n" *)
    (*     (List.split_by_comma layout_constant cs) *)
    (*     (layout_haft SFA.layout_raw_regex tau) *)
    (* in *)
    match (tau, cs) with
    | RtyHAParallel { history; parallel; _ }, [] ->
        if SFA.is_match reduce_sevent history runtime.trace then [ parallel ]
        else []
    | RtyGArr { arg; argnty; retrty }, cs ->
        let samples = SampleDomain.find argnty runtime.sample_domain in
        let ress =
          List.concat_map
            (fun c ->
              let retrty = subst_pat arg (AC c) retrty in
              aux (retrty, cs))
            samples
        in
        ress
    | RtyArr { arg; argcty; retrty }, c :: cs ->
        if reduce_cty c argcty then
          let retrty = subst_pat arg (AC c) retrty in
          aux (retrty, cs)
        else []
    | RtyInter (tau1, tau2), _ -> aux (tau1, cs) @ aux (tau2, cs)
    | _, _ -> _die [%here]
  in
  match aux (tau, cs) with [] -> _die [%here] | l -> choose_from_list l

let sample_phi runtime (vs, prop) =
  let rec aux (n : int) =
    if n <= 0 then
      let () =
        Printf.printf "vs: %s; prop: %s\n" (layout_qvs vs) (layout_prop prop)
      in
      _die_with [%here] "sample too many times"
    else
      let store = List.map (sample runtime) vs in
      let store' = StrMap.add_seq (List.to_seq store) runtime.store in
      if eval_prop store' prop then store (* List.map snd store *)
      else aux (n - 1)
  in
  aux 10000

let mk_assume runtime (vs, prop) =
  let store = sample_phi runtime (vs, prop) in
  let store' = StrMap.add_seq (List.to_seq store) runtime.store in
  { runtime with store = store' }

let sample_event runtime = function
  | { op; vs; phi } ->
      let store = sample_phi runtime (vs, phi) in
      let cs = List.map snd store in
      (op, cs)

let send runtime (op, cs) =
  let tau = _get_force [%here] runtime.event_rtyctx op in
  let ses = reduce_pat runtime cs tau in
  let msgs = List.map (sample_event runtime) ses in
  {
    runtime with
    trace = runtime.trace @ [ (op, cs) ];
    buffer = runtime.buffer @ msgs;
  }

let recv_and_send runtime op (lhs, prop) =
  let avialable_msgs =
    List.filter_mapi
      (fun idx (op', args) ->
        let rest =
          List.sublist runtime.buffer ~start_included:0 ~end_excluded:idx
          @ List.sublist runtime.buffer ~start_included:(idx + 1)
              ~end_excluded:(List.length runtime.buffer)
        in
        if String.equal op op' then Some (args, rest) else None)
      runtime.buffer
  in
  let avialable_msgs =
    List.filter
      (fun (cs, _) ->
        let store = store_add (lhs, cs) runtime.store in
        eval_prop store prop)
      avialable_msgs
  in
  let args, buffer =
    match avialable_msgs with
    | [] ->
        let () =
          Pp.printf
            "@{<red>@{<bold>runtime error:@}@} cannot recv message(%s)\n" op
        in
        raise (RuntimeInconsistent runtime)
    | _ -> choose_from_list avialable_msgs
  in
  (* let rec aux = function *)
  (*   | [] -> *)
  (*       let () = *)
  (*         Pp.printf *)
  (*           "@{<red>@{<bold>runtime error:@}@} cannot recv message(%s)\n" op *)
  (*       in *)
  (*       raise (RuntimeInconsistent runtime) *)
  (*   | ((op', args) as elem) :: buffer -> *)
  (*       if String.equal op op' then (args, buffer) *)
  (*       else *)
  (*         let args, buffer = aux buffer in *)
  (*         (args, elem :: buffer) *)
  (* in *)
  (* let args, buffer = aux runtime.buffer in *)
  let runtime = send { runtime with buffer } (op, args) in
  (runtime, args)

let gen runtime elem = { runtime with buffer = runtime.buffer @ [ elem ] }
