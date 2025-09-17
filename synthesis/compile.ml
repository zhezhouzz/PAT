open Language
open Zdatatype
open Plan
(* open Common *)

module SimpleRename = struct
  let _var_counter = ref 0
  let default_gen_var = "__x"
  let default_obs_var = "__y"
  let init () = _var_counter := 0

  let new_gen_var x =
    let fresh_var = spf "%s%i" default_gen_var !_var_counter in
    _var_counter := !_var_counter + 1;
    fresh_var#:x.ty

  let new_obs_var x =
    let fresh_var = spf "%s%i" default_obs_var !_var_counter in
    _var_counter := !_var_counter + 1;
    fresh_var#:x.ty
end

open SimpleRename

let act_to_term env { aop; aargs; _ } =
  if is_gen env aop then
    let args = List.map (fun x -> VVar x) aargs in
    mk_term_gen env.event_tyctx aop args
  else if is_obs env aop then mk_term_obs env.event_tyctx aop aargs mk_true
  else _die [%here]

let rec acts_to_term env = function
  | [] -> mk_term_tt
  | act :: acts -> act_to_term env act @@ acts_to_term env acts

let normalize_line env { gprop; elems } =
  let () = SimpleRename.init () in
  let rec aux gen_vars obs_vars (gprop, acts) = function
    | [] -> (gen_vars, obs_vars, (gprop, acts))
    | LineAct act :: post ->
        if is_gen env act.aop then
          let aargs' = List.map new_gen_var act.aargs in
          let gprop = msubst_prop (act.aargs, aargs') gprop in
          let act = { act with aargs = aargs' } in
          aux (gen_vars @ aargs') obs_vars (gprop, acts @ [ act ]) post
        else if is_obs env act.aop then
          let aargs' = List.map new_obs_var act.aargs in
          let gprop = msubst_prop (act.aargs, aargs') gprop in
          let act = { act with aargs = aargs' } in
          aux gen_vars (obs_vars @ aargs') (gprop, acts @ [ act ]) post
        else _die [%here]
    | LineStarMultiChar _ :: post -> aux gen_vars obs_vars (gprop, acts) post
  in
  let gen_vars, obs_vars, (gprop, acts) = aux [] [] (gprop, []) elems in
  (* let gprop = LineOpt.optimize_prop (gen_vars @ obs_vars, gprop) in *)
  let prog = acts_to_term env acts in
  (gen_vars, obs_vars, gprop, prog)

let append_post term post =
  let rec aux = function
    | CLetE { lhs; rhs; body } ->
        CLetE { lhs; rhs; body = (aux body.x)#:body.ty }
    | CVal _ -> CAssertP post
    | _ -> _die [%here]
  in
  aux term

let distribute_assumption (qvs, term, gprop) =
  (* let rec gather_adj_gen term =
    match term with
    | CLetE { lhs; rhs; body } -> (
        match rhs with
        | { x = CGen { args; _ }; _ } ->
            let gvars, k, term = gather_adj_gen body.x in
            let args =
              List.map
                (fun x -> match x.x with VVar x -> x | _ -> _die [%here])
                args
            in
            let gvars = gvars @ args in
            let k = fun e -> CLetE { lhs; rhs; body = (k e)#:body.ty } in
            (gvars, k, term)
        | _ -> ([], (fun e -> e), body.x))
    | CVal _ -> ([], (fun e -> e), term)
    | _ -> _die [%here]
  in *)
  let rec aux lvars (term, gprop) =
    match term with
    | CLetE { lhs; rhs; body } -> (
        match rhs with
        | { x = CGen { args; _ }; _ } ->
            let args =
              List.map
                (fun x -> match x.x with VVar x -> x | _ -> _die [%here])
                args
            in
            let args =
              List.filter
                (fun x ->
                  not (List.exists (fun y -> String.equal x.x y.x) lvars))
                args
            in
            let lvars = lvars @ args in
            let pre = Abduction.simp_abduction (lvars, args, gprop) in
            let post = Abduction.pre_simplify_prop (qvs, pre) gprop in
            let body', gprop = aux lvars (body.x, post) in
            let term = CLetE { lhs; rhs; body = body'#:body.ty } in
            (mk_term_assume args pre term, gprop)
        | { x = CObs { prop; _ }; _ } ->
            let lvars = lvars @ lhs in
            let body', gprop = aux lvars (body.x, smart_add_to prop gprop) in
            (CLetE { lhs; rhs; body = body'#:body.ty }, gprop)
        | _ -> _die [%here])
    | CVal _ -> (term, gprop)
    | _ -> _die [%here]
  in
  aux [] (term, gprop)

let compile_term_from_line env e =
  let gen_vars, obs_vars, gprop, prog = normalize_line env e in
  let tmp_vars =
    List.filter
      (fun x ->
        not (List.exists (fun y -> String.equal x.x y.x) (gen_vars @ obs_vars)))
      (fv_prop gprop)
  in
  let _, gprop, prog = SimpEq.simp (List.map _get_x tmp_vars, gprop, prog) in
  let gprop, prog = SimpEq.mk_eq_from_prev (gprop, prog) in
  let prog, post = distribute_assumption (gen_vars @ obs_vars, prog, gprop) in
  let () = Pp.printf "@{<bold>prog:@}\n%s\n" (layout_term prog) in
  let () = Pp.printf "@{<bold>post:@}\n%s\n" (layout_prop post) in
  prog

let add_kstar (pre_len, length) e =
  let () = Pp.printf "@{<bold>add_kstar@}\n" in
  let () = Pp.printf "@{<bold>pre_len:@}\n%i\n" pre_len in
  let () = Pp.printf "@{<bold>length:@}\n%i\n" length in
  let () = Pp.printf "@{<bold>e:@}\n%s\n" (layout_term e.x) in
  let rec add length (k, term) =
    if length == 0 then
      let body = k mk_term_tt#:Nt.unit_ty in
      (CLetE
         {
           lhs = [];
           rhs = { x = KStar { body }; ty = Nt.unit_ty };
           body = term;
         })#:Nt.unit_ty
    else
      match term.x with
      | CLetE { lhs; rhs = { x = CAssume _; _ } as rhs; body } ->
          let k = fun e -> k (CLetE { lhs; rhs; body = e })#:Nt.unit_ty in
          add length (k, body)
      | CLetE { lhs; rhs; body } ->
          let k = fun e -> k (CLetE { lhs; rhs; body = e })#:Nt.unit_ty in
          add (length - 1) (k, body)
      | _ -> _die [%here]
  in
  let rec aux pre_len term =
    if pre_len == 0 then add length ((fun e -> e), term)
    else
      match term.x with
      | CLetE { lhs; rhs = { x = CAssume _; _ } as rhs; body } ->
          let body' = aux pre_len body in
          (CLetE { lhs; rhs; body = body' })#:term.ty
      | CLetE { lhs; rhs; body } ->
          let body' = aux (pre_len - 1) body in
          (CLetE { lhs; rhs; body = body' })#:term.ty
      | _ ->
          let () = Pp.printf "@{<bold>term:@}\n%s\n" (layout_term term.x) in
          _die [%here]
  in
  aux pre_len e

let compile_term env e =
  match e with
  | SynMidPlan line -> compile_term_from_line env line
  | SynMidKStar (pre_len, line, post_len) ->
      let e = compile_term_from_line env line in
      let size = line_size line in
      let length = size - pre_len - post_len in
      let res = add_kstar (pre_len, length) e#:(term_to_nt e) in
      res.x
