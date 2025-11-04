open Language
open Zdatatype
(* open Plan *)
(* open Common *)

let recursion_vars = [ default_bound_var.x; default_iter_var.x ]

module SimpleRename = struct
  let _var_counter = ref 0
  let default_gen_var = "__x"
  let default_obs_var = "__y"
  let init () = _var_counter := 0
  let preserved_vars = ref recursion_vars
  let add_preserved_var l = preserved_vars := !preserved_vars @ l

  let new_gen_var used_vars x =
    if List.exists (String.equal x.x) (!preserved_vars @ used_vars) then x
    else
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

let unique_name_line env { gprop; elems } =
  let rec aux used_names (gprop, acts) = function
    | [] -> { gprop; elems = acts }
    | LineAct act :: post ->
        if is_gen env act.aop then
          let new_names = List.map (fun x -> x.x) act.aargs in
          aux (used_names @ new_names) (gprop, acts @ [ LineAct act ]) post
        else
          let f x = (Rename.unique_var "ttmp")#:x.ty in
          let aargs' = List.map f act.aargs in
          let m =
            List.map
              (fun (x, y) -> (x.x, y))
              (_safe_combine [%here] act.aargs aargs')
          in
          let () =
            Pp.printf "@{<bold>unique name:@}\n%s\n"
              (List.split_by_comma (fun (x, y) -> spf "%s -> %s" x y.x) m)
          in
          let prop =
            List.filter_map (fun (x, y) ->
                if String.equal x.x y.x then None
                else
                  let lit = mk_var_eq_var [%here] x y in
                  Some (lit_to_prop lit))
            @@ _safe_combine [%here] act.aargs aargs'
          in
          let prop = smart_and prop in
          let gprop = smart_and [ gprop; prop ] in
          let act = { act with aargs = aargs' } in
          let new_names = List.map (fun x -> x.x) aargs' in
          aux (used_names @ new_names)
            (gprop, acts @ [ LineAct { act with aargs = aargs' } ])
            post
    | (LineStarMultiChar _ as elem) :: post ->
        aux used_names (gprop, acts @ [ elem ]) post
  in
  if Prover.check_sat_bool (None, gprop) then aux [] (gprop, []) elems
  else _die_with [%here] (spf "not pass sanity check (%s)" (layout_prop gprop))

let normalize_line env line =
  let { gprop; elems } = unique_name_line env line in
  let () =
    Pp.printf "@{<bold>unique line:@}\n%s\n"
      (Plan.omit_layout_line { gprop; elems })
  in
  let rec aux gen_vars obs_vars (gprop, acts) elems =
    let () =
      Pp.printf "@{<bold>aux@} (%s, %s)\nprop: %s\nelems: %s\n"
        (List.split_by_comma (fun x -> x.x) gen_vars)
        (List.split_by_comma (fun x -> x.x) obs_vars)
        (layout_prop gprop)
        (Plan.omit_layout_line_elems elems)
    in
    match elems with
    | [] -> (gen_vars, obs_vars, (gprop, acts))
    | LineAct act :: post ->
        if is_gen env act.aop then
          let aargs' =
            List.map
              (new_gen_var (List.map _get_x (gen_vars @ obs_vars)))
              act.aargs
          in
          let m =
            List.map
              (fun (x, y) -> (x.x, y))
              (_safe_combine [%here] act.aargs aargs')
          in
          let gprop = msubst_prop (act.aargs, aargs') gprop in
          let act = { act with aargs = aargs' } in
          let () =
            Pp.printf "@{<bold>m:@}\n%s\n"
              (List.split_by_comma (fun (x, y) -> spf "%s -> %s" x y.x) m)
          in
          let post = List.map (msubst subst_name_in_line_elem m) post in
          aux (gen_vars @ aargs') obs_vars (gprop, acts @ [ act ]) post
        else if is_obs env act.aop then
          let aargs' = List.map new_obs_var act.aargs in
          let m =
            List.map
              (fun (x, y) -> (x.x, y))
              (_safe_combine [%here] act.aargs aargs')
          in
          let gprop = msubst_prop (act.aargs, aargs') gprop in
          let act = { act with aargs = aargs' } in
          let post = List.map (msubst subst_name_in_line_elem m) post in
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
            let args =
              List.filter
                (fun x -> not (List.exists (String.equal x.x) recursion_vars))
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
  let () = Pp.printf "@{<bold>prog1:@}\n%s\n" (layout_term prog) in
  let () = Pp.printf "@{<bold>gprop1:@}\n%s\n" (layout_prop gprop) in
  let prog, post = distribute_assumption (gen_vars @ obs_vars, prog, gprop) in
  let () = Pp.printf "@{<bold>prog:@}\n%s\n" (layout_term prog) in
  let () = Pp.printf "@{<bold>post:@}\n%s\n" (layout_prop post) in
  prog

let add_kstar (pre_len, length) e rec_branch_1 rec_branch_2 =
  let () = Pp.printf "@{<bold>add_kstar@}\n" in
  let () = Pp.printf "@{<bold>pre_len:@}\n%i\n" pre_len in
  let () = Pp.printf "@{<bold>length:@}\n%i\n" length in
  let () = Pp.printf "@{<bold>e:@}\n%s\n" (layout_term e.x) in
  let mkFixApp b1 e = mk_rec b1 rec_branch_2 (mk_rec_app_0 e) in
  let rec add length (k, term) =
    if length == 0 then mkFixApp rec_branch_1 term.x
    else
      match term.x with
      | CLetE { lhs; rhs = { x = CAssume _; _ } as rhs; body } ->
          let k = fun e -> k (CLetE { lhs; rhs; body = e#:(term_to_nt e) }) in
          add length (k, body)
      | CLetE { lhs; rhs; body } ->
          let k = fun e -> k (CLetE { lhs; rhs; body = e#:(term_to_nt e) }) in
          add (length - 1) (k, body)
      | _ -> _die [%here]
  in
  let rec aux pre_len term =
    if pre_len == 0 then add length ((fun e -> e), term)
    else
      match term.x with
      | CLetE { lhs; rhs = { x = CAssume _; _ } as rhs; body } ->
          let body' = aux pre_len body in
          CLetE { lhs; rhs; body = body'#:(term_to_nt body') }
      | CLetE { lhs; rhs; body } ->
          let body' = aux (pre_len - 1) body in
          CLetE { lhs; rhs; body = body'#:(term_to_nt body') }
      | _ ->
          let () = Pp.printf "@{<bold>term:@}\n%s\n" (layout_term term.x) in
          _die [%here]
  in
  aux pre_len e

let add_kstar_drop pre_len e rec_branch_1 rec_branch_2 v =
  let () = Pp.printf "@{<bold>add_kstar@}\n" in
  let () = Pp.printf "@{<bold>pre_len:@}\n%i\n" pre_len in
  let () = Pp.printf "@{<bold>e:@}\n%s\n" (layout_term e.x) in
  let mkFixApp b1 e = mk_rec b1 rec_branch_2 (mk_rec_app_v v e) in
  let rec aux pre_len term =
    if pre_len == 0 then mkFixApp rec_branch_1 term.x
    else
      match term.x with
      | CLetE { lhs; rhs = { x = CAssume _; _ } as rhs; body } ->
          let body' = aux pre_len body in
          CLetE { lhs; rhs; body = body'#:(term_to_nt body') }
      | CLetE { lhs; rhs; body } ->
          let body' = aux (pre_len - 1) body in
          CLetE { lhs; rhs; body = body'#:(term_to_nt body') }
      | _ ->
          let () = Pp.printf "@{<bold>term:@}\n%s\n" (layout_term term.x) in
          _die [%here]
  in
  aux pre_len e

let mk_fix_body pre_len e =
  let rec aux pre_len term =
    if pre_len == 0 then mk_rec_app_incr term.x
    else
      match term.x with
      | CLetE { lhs; rhs = { x = CAssume _; _ } as rhs; body } ->
          let body' = aux pre_len body in
          CLetE { lhs; rhs; body = body'#:(term_to_nt body') }
      | CLetE { lhs; rhs; body } ->
          let body' = aux (pre_len - 1) body in
          CLetE { lhs; rhs; body = body'#:(term_to_nt body') }
      | _ ->
          let () = Pp.printf "@{<bold>term:@}\n%s\n" (layout_term term.x) in
          _die [%here]
  in
  aux pre_len e

let rec simplfily_assumption_aux (vars, prop, e) =
  match vars with
  | [] -> ([], prop, e)
  | var :: vars -> (
      let vars, prop, e = simplfily_assumption_aux (vars, prop, e) in
      match Prop.SimplProp.find_eq_lit_in_prop var.x prop with
      | None -> (var :: vars, prop, e)
      | Some lit -> (
          match lit.x with
          | AC _ ->
              let prop = subst_prop_instance var.x lit.x prop in
              let e = subst_term_instance var.x lit.x e in
              (vars, prop, e)
          | _ -> (var :: vars, prop, e)))

let simplfily_assumption e =
  let rec aux term =
    match term with
    | CLetE { lhs; rhs = { x = CAssume (_, prop); ty }; body } -> (
        let body' = typed_aux body in
        let vars, prop, body'' =
          simplfily_assumption_aux (lhs, prop, body'.x)
        in
        match vars with
        | [] -> body''
        | _ ->
            CLetE
              {
                lhs = vars;
                rhs = { x = CAssume (List.map _get_ty vars, prop); ty };
                body = term_to_tterm body'';
              })
    | CLetE { lhs; rhs = { x = CFix _; _ } as rhs; body } ->
        let body' = typed_aux body in
        let rhs = typed_aux rhs in
        CLetE { lhs; rhs; body = body' }
    | CLetE { lhs; rhs; body } ->
        let body' = typed_aux body in
        CLetE { lhs; rhs; body = body' }
    | CVal _ | CAppOp _ | CAssume _ | CAssertP _ -> term
    | CUnion es -> CUnion (List.map typed_aux es)
    | CFix { retBranch; recBranch } ->
        CFix
          { retBranch = typed_aux retBranch; recBranch = typed_aux recBranch }
    | CFixApp { cfix; iterV; boundV } ->
        let cfix =
          match cfix with Some cfix -> Some (typed_aux cfix) | None -> None
        in
        CFixApp { cfix; iterV; boundV }
    | _ -> _die [%here]
  and typed_aux { x; ty } : (Nt.nt, term) typed = { x = aux x; ty } in
  aux e

let remove_unused_assume e =
  let rec aux term =
    match term.x with
    | CLetE { lhs; rhs = { x = CAssume _; _ } as rhs; body } ->
        let body' = aux body in
        let fvs = fv_term body'.x in
        let inter = List.interset (fun x y -> String.equal x.x y.x) fvs lhs in
        if 0 == List.length inter then body'
        else term_to_tterm (CLetE { lhs; rhs; body = body' })
    | CLetE { lhs; rhs = { x = _; _ } as rhs; body } ->
        let rhs = aux rhs in
        let body' = aux body in
        term_to_tterm (CLetE { lhs; rhs; body = body' })
    | CVal _ | CAssertP _ | CObs _ | CGen _ | CAppOp _ -> term
    | CAssume _ -> _die [%here]
    | CUnion es -> term_to_tterm (CUnion (List.map aux es))
    | CFix { retBranch; recBranch } ->
        let retBranch = aux retBranch in
        let recBranch = aux recBranch in
        term_to_tterm (CFix { retBranch; recBranch })
    | CFixApp { cfix; iterV; boundV } ->
        let cfix =
          match cfix with Some cfix -> Some (aux cfix) | None -> None
        in
        term_to_tterm (CFixApp { cfix; iterV; boundV })
  in
  aux e

let postpone_assume e =
  let rec aux (vars, k) term =
    match term.x with
    | CLetE { lhs; rhs = { x = CAssume _; _ } as rhs; body } ->
        let fvs = fv_term body.x in
        let inter = List.interset (fun x y -> String.equal x.x y.x) fvs lhs in
        if 0 == List.length inter then aux (vars, k) body
        else
          let k body = k (term_to_tterm (CLetE { lhs; rhs; body })) in
          aux (vars @ List.map _get_x lhs, k) body
    | CLetE
        {
          lhs;
          rhs =
            {
              x = CObs _ | CGen _ | CVal _ | CAppOp _ | CAssertP _ | CFixApp _;
              _;
            } as rhs;
          body;
        } ->
        let fvs = List.map _get_x (fv_term rhs.x) in
        let () =
          Pp.printf "@{<bold>vars:@}%s\n@{<bold>fvs:@}%s => %s\n"
            (List.split_by_comma (fun x -> x) vars)
            (layout_term rhs.x)
            (List.split_by_comma (fun x -> x) fvs)
        in
        if 0 == List.length (List.interset String.equal fvs vars) then
          let body' = aux (vars, k) body in
          term_to_tterm (CLetE { lhs; rhs; body = body' })
        else
          let body' = aux ([], fun e -> e) body in
          k (term_to_tterm (CLetE { lhs; rhs; body = body' }))
    | CLetE { lhs; rhs = { x = CFix _; _ } as rhs; body } ->
        let rhs = aux ([], fun e -> e) rhs in
        let body' = aux (vars, k) body in
        term_to_tterm (CLetE { lhs; rhs; body = body' })
    | CLetE { lhs; rhs; body } ->
        let rhs = aux (vars, k) rhs in
        let body' = aux ([], fun e -> e) body in
        term_to_tterm (CLetE { lhs; rhs; body = body' })
    | CVal _ -> term
    | CAssume _ | CAssertP _ | CObs _ | CGen _ | CAppOp _ -> _die [%here]
    | CUnion es -> term_to_tterm (CUnion (List.map (aux (vars, k)) es))
    | CFix { retBranch; recBranch } ->
        let retBranch = aux ([], fun e -> e) retBranch in
        let recBranch = aux ([], fun e -> e) recBranch in
        term_to_tterm (CFix { retBranch; recBranch })
    | CFixApp { cfix; iterV; boundV } ->
        let cfix =
          match cfix with
          | Some cfix -> Some (aux ([], fun e -> e) cfix)
          | None -> None
        in
        term_to_tterm (CFixApp { cfix; iterV; boundV })
  in
  aux e

(* let simp_assumption_prop (lvars, args, prop, e) =
  match get_assign_close_lits lvars prop args with
  | None -> (args, prop)
  | Some ass ->
      let prop = msubst subst_prop_instance ass prop in
      let e = msubst subst_term_instance ass e in
      line *)

let merge_assume e =
  let rec aux term =
    match term with
    | CLetE { lhs; rhs = { x = CAssume (args, prop); _ } as rhs; body } -> (
        let body' = typed_aux body in
        match body'.x with
        | CLetE
            {
              lhs = lhs';
              rhs = { x = CAssume (args', prop'); ty };
              body = body';
            } ->
            let rhs = (CAssume (args @ args', smart_add_to prop prop'))#:ty in
            let body = CLetE { lhs = lhs' @ lhs; rhs; body = body' } in
            body
        | _ -> CLetE { lhs; rhs; body = body' })
    | CLetE { lhs; rhs = { x = CFix _; _ } as rhs; body } ->
        let body' = typed_aux body in
        let rhs = typed_aux rhs in
        CLetE { lhs; rhs; body = body' }
    | CLetE { lhs; rhs; body } ->
        let body' = typed_aux body in
        CLetE { lhs; rhs; body = body' }
    | CVal _ | CAppOp _ | CAssume _ | CAssertP _ -> term
    | CUnion es -> CUnion (List.map typed_aux es)
    | CFix { retBranch; recBranch } ->
        CFix
          { retBranch = typed_aux retBranch; recBranch = typed_aux recBranch }
    | CFixApp { cfix; iterV; boundV } ->
        let cfix =
          match cfix with Some cfix -> Some (typed_aux cfix) | None -> None
        in
        CFixApp { cfix; iterV; boundV }
    | _ -> _die [%here]
  and typed_aux { x; ty } = { x = aux x; ty } in
  aux e

let drop_ghost_events e =
  let rec aux term =
    match term with
    | CLetE { lhs; rhs = { x = CGen { op; _ }; _ } as rhs; body } ->
        let body' = typed_aux body in
        if List.exists (String.equal op.x) ghost_event_names then body'.x
        else CLetE { lhs; rhs; body = body' }
    | CLetE { lhs; rhs = { x = CObs { op; _ }; _ } as rhs; body } ->
        let body' = typed_aux body in
        if List.exists (String.equal op.x) ghost_event_names then body'.x
        else CLetE { lhs; rhs; body = body' }
    | CLetE { lhs; rhs = { x = CFix _; _ } as rhs; body } ->
        let body' = typed_aux body in
        let rhs = typed_aux rhs in
        CLetE { lhs; rhs; body = body' }
    | CLetE { lhs; rhs; body } ->
        let body' = typed_aux body in
        CLetE { lhs; rhs; body = body' }
    | CVal _ | CAppOp _ | CAssume _ | CAssertP _ -> term
    | CUnion es -> CUnion (List.map typed_aux es)
    | CFix { retBranch; recBranch } ->
        CFix
          { retBranch = typed_aux retBranch; recBranch = typed_aux recBranch }
    | CFixApp { cfix; iterV; boundV } ->
        let cfix =
          match cfix with Some cfix -> Some (typed_aux cfix) | None -> None
        in
        CFixApp { cfix; iterV; boundV }
    | _ -> _die [%here]
  and typed_aux { x; ty } = { x = aux x; ty } in
  aux e

let compile_term env e =
  let () = SimpleRename.init () in
  match e with
  | SynMidPlan line -> compile_term_from_line env line
  | SynMidKStar { old_goal; pre_len; line_b1; line_b2; line_b2_pre_len; v } ->
      let () = SimpleRename.add_preserved_var (List.map _get_x (fv_value v)) in
      let () = Pp.printf "@{<bold>compiled term:@}\n" in
      let e = compile_term_from_line env old_goal in
      let () = Pp.printf "@{<bold>rec_branch_2:@}\n" in
      let rec_branch_2 = compile_term_from_line env line_b2 in
      let () = Pp.printf "@{<bold>rec_branch_1:@}\npre_len: %i\n" pre_len in
      let rec_branch_1 = compile_term_from_line env line_b1 in
      let rec_branch_2 =
        mk_fix_body line_b2_pre_len (term_to_tterm rec_branch_2)
      in
      let () = Pp.printf "@{<bold>v:@}\n%s\n" (layout_value v) in
      let res =
        add_kstar_drop pre_len e#:(term_to_nt e) rec_branch_1 rec_branch_2 v
      in
      let () = Pp.printf "@{<bold>result term:@}\n%s\n" (layout_term res) in
      let res = drop_ghost_events res in
      let () =
        Pp.printf "@{<bold>result term after dropping ghost events:@}\n%s\n"
          (layout_term res)
      in
      let res = remove_unused_assume (term_to_tterm res) in
      let () =
        Pp.printf "@{<bold>result term after removing unused assume:@}\n%s\n"
          (layout_term res.x)
      in
      let res = postpone_assume ([], fun e -> e) res in
      let () =
        Pp.printf "@{<bold>result term after postponing assume:@}\n%s\n"
          (layout_term res.x)
      in
      let res = simplfily_assumption res.x in
      let () =
        Pp.printf "@{<bold>result term after simplifying assumption:@}\n%s\n"
          (layout_term res)
      in
      let res = merge_assume res in
      let () =
        Pp.printf "@{<bold>result term after merging assume:@}\n%s\n"
          (layout_term res)
      in
      res
