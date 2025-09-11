open Language
open Zdatatype

(** Shift a term by i with a cutoff c *)
let rec shift_stlcTerm c i = function
  | StlcVar n -> if n < c then StlcVar n else StlcVar (n + i)
  | StlcConst c -> StlcConst c
  | StlcAbs { absTy; absBody } ->
      StlcAbs { absTy; absBody = shift_stlcTerm (c + 1) i absBody }
  | StlcApp { appFun; appArg } ->
      StlcApp
        {
          appFun = shift_stlcTerm c i appFun;
          appArg = shift_stlcTerm c i appArg;
        }

let layout_stlcCtx ctx =
  List.split_by_comma (fun x -> x)
  @@ List.mapi (fun i ty -> spf "x%d : %s" i (layout_stlcTy ty)) ctx

let find_stlcTy ctx x =
  (* let () = Printf.printf "find_stlcTy %d\n" x in *)
  (* let () = Printf.printf "ctx: %s\n" (layout_stlcCtx ctx) in *)
  let ty = List.nth ctx x in
  (* let () = Printf.printf "ty: %s\n" (layout_stlcTy ty) in *)
  ty

let rec typeinfer_stlcTerm ctx e =
  let type_error () =
    let () =
      Printf.printf "typeinfer_stlcTerm %s |- %s\n" (layout_stlcCtx ctx)
        (layout_stlcTerm e)
    in
    None
  in
  let res =
    match e with
    | StlcVar x ->
        if x >= List.length ctx then type_error () else Some (find_stlcTy ctx x)
    | StlcConst _ -> Some StlcInt
    | StlcAbs { absTy; absBody } -> (
        let ty = typeinfer_stlcTerm (absTy :: ctx) absBody in
        match ty with Some ty -> Some (StlcArrow (absTy, ty)) | None -> None)
    | StlcApp { appFun; appArg } -> (
        let ty1 = typeinfer_stlcTerm ctx appFun in
        let ty2 = typeinfer_stlcTerm ctx appArg in
        match (ty1, ty2) with
        | None, _ | _, None -> None
        | Some (StlcArrow (ty11, ty12)), Some ty2 ->
            if equal_stlcTy ty11 ty2 then Some ty12 else type_error ()
        | _ -> type_error ())
  in
  let () =
    Printf.printf "typeinfer_stlcTerm %s |- %s: %s\n" (layout_stlcCtx ctx)
      (layout_stlcTerm e)
      (match res with None -> "None" | Some ty -> layout_stlcTy ty)
  in
  res

let tyinfer_from_empty e = typeinfer_stlcTerm [] e

let rec subst_stlcTerm (n, e) = function
  | StlcVar x -> if x == n then e else StlcVar x
  | StlcConst c -> StlcConst c
  | StlcAbs { absTy; absBody } ->
      StlcAbs
        {
          absTy;
          absBody = subst_stlcTerm (n + 1, shift_stlcTerm 0 1 e) absBody;
        }
  | StlcApp { appFun; appArg } ->
      StlcApp
        {
          appFun = subst_stlcTerm (n, e) appFun;
          appArg = subst_stlcTerm (n, e) appArg;
        }

let rec wrong_subst (n, e) = function
  | StlcVar x -> if x == n then e else StlcVar x
  | StlcConst c -> StlcConst c
  | StlcAbs { absTy; absBody } ->
      StlcAbs { absTy; absBody = wrong_subst (n, e) absBody }
  | StlcApp { appFun; appArg } ->
      StlcApp
        {
          appFun = wrong_subst (n, e) appFun;
          appArg = wrong_subst (n, e) appArg;
        }

type eval_result =
  | EvalSuccess of stlcTerm
  | EvalError of stlcTerm
  | EvalNormalFrom of stlcTerm

let rec step_stlcTerm term =
  match term with
  | StlcApp { appFun = StlcAbs { absBody; absTy }; appArg } -> (
      match step_stlcTerm appArg with
      | EvalError _ -> EvalError term
      | EvalNormalFrom appArg ->
          (* let e =
            shift_stlcTerm 0 (-1)
            @@ subst_stlcTerm (0, shift_stlcTerm 0 1 appArg) absBody
          in *)
          let e = shift_stlcTerm 0 (-1) (wrong_subst (0, appArg) absBody) in
          (* let e = subst_stlcTerm (0, appArg) (shift_stlcTerm 0 (-1) absBody) in *)
          EvalSuccess e
      | EvalSuccess e ->
          EvalSuccess
            (StlcApp { appFun = StlcAbs { absBody; absTy }; appArg = e }))
  | StlcApp { appFun; appArg } -> (
      match step_stlcTerm appFun with
      | EvalError _ -> EvalError term
      | EvalNormalFrom _ -> EvalError term
      | EvalSuccess e -> EvalSuccess (StlcApp { appFun = e; appArg }))
  | _ -> EvalNormalFrom term

let rec mstep_stlcTerm e =
  let () = Printf.printf "mstep_stlcTerm: %s\n" (layout_stlcTerm e) in
  match step_stlcTerm e with
  | EvalError _ -> EvalError e
  | EvalNormalFrom e -> EvalNormalFrom e
  | EvalSuccess e -> mstep_stlcTerm e

open Interpreter

module EvaluationCtx = struct
  type t = MkAbs of stlcTy | MkAppL | MkAppR of stlcTerm

  let _sid = ref 0

  let next_sid () =
    let sid = !_sid in
    _sid := sid + 1;
    sid

  let _tmp = ref (StlcConst 0)
  let eCtx = ref []

  let ec_init () =
    _sid := 0;
    _tmp := StlcConst 0;
    eCtx := []

  let layout ctx =
    let rec aux = function
      | [] -> "□"
      | MkAbs ty :: ctx -> spf "\\(%s).%s" (layout_stlcTy ty) (aux ctx)
      | MkAppL :: ctx -> spf "(%s □)" (aux ctx)
      | MkAppR e :: ctx -> spf "(%s %s)" (layout_stlcTerm e) (aux ctx)
    in
    aux (List.rev ctx)

  let sendCurTy ty =
    let () = Printf.printf "tmp: %s\n" (layout_stlcTerm !_tmp) in
    let () = Printf.printf "ECtx: %s\n" (layout !eCtx) in
    sendTo (None, { op = "curTy"; args = [ mk_value_stlcTy ty ] })

  let step e = function
    | [] -> _die_with [%here] "not a well-formed term"
    | MkAbs absTy :: ctx ->
        _tmp := StlcAbs { absTy; absBody = e };
        ctx
    | MkAppL :: ctx -> MkAppR e :: ctx
    | MkAppR e1 :: ctx ->
        _tmp := StlcApp { appFun = e1; appArg = e };
        ctx

  let mkConHandler (_ : msg) =
    _tmp := StlcConst (Random.int 10);
    sendCurTy StlcInt

  (* let findTyInCtx ctx x =
    let rec aux n = function
      | [] -> _die_with [%here] "variable not found"
      | MkAbs absTy :: ctx -> if n == 0 then absTy else aux (n - 1) ctx
      | _ :: ctx -> aux n ctx
    in
    aux x ctx *)

  let eCtx2Ctx ctx =
    let rec aux = function
      | [] -> []
      | MkAbs ty :: ctx -> ty :: aux ctx
      | _ :: ctx -> aux ctx
    in
    aux ctx

  let mkVarHandler (msg : msg) =
    let x =
      match msg.ev.args with [ VConst (I x) ] -> x | _ -> _die [%here]
    in
    _tmp := StlcVar x;
    sendCurTy (find_stlcTy (eCtx2Ctx !eCtx) x)

  let mkAbsHandler (msg : msg) =
    let ty =
      match msg.ev.args with [ VCStlcTy ty ] -> ty | _ -> _die [%here]
    in
    eCtx := MkAbs ty :: !eCtx;
    send ("closureId", [ mk_value_int (next_sid ()) ])

  let closeAbsHandler (_ : msg) =
    match !eCtx with
    | [] -> _die_with [%here] "not a well-formed term"
    | _ -> (
        eCtx := step !_tmp !eCtx;
        match typeinfer_stlcTerm (eCtx2Ctx !eCtx) !_tmp with
        | Some ty -> sendCurTy ty
        | None -> ())

  let mkAppHandler (_ : msg) =
    eCtx := MkAppL :: !eCtx;
    send ("closureId", [ mk_value_int (next_sid ()) ])

  let closureIdHandler (_ : msg) = ()

  let closeAppLHandler (_ : msg) =
    match !eCtx with
    | [] -> _die_with [%here] "not a well-formed term"
    | _ -> eCtx := step !_tmp !eCtx

  let closeAppRHandler (_ : msg) =
    match !eCtx with
    | [] -> _die_with [%here] "not a well-formed term"
    | _ -> (
        eCtx := step !_tmp !eCtx;
        match typeinfer_stlcTerm (eCtx2Ctx !eCtx) !_tmp with
        | Some ty -> sendCurTy ty
        | None -> ())

  let curTyHandler (_ : msg) = ()

  let evalReqHandler (_ : msg) =
    let res = mstep_stlcTerm !_tmp in
    match res with
    | EvalNormalFrom _ -> send ("evalResp", [ mk_value_bool true ])
    | EvalError _ -> send ("evalResp", [ mk_value_bool false ])
    | EvalSuccess _ -> send ("evalResp", [ mk_value_bool false ])

  let evalRespHandler (_ : msg) = ()
end

open EvaluationCtx

let init () =
  Interpreter.init ();
  register_handler "mkCon" mkConHandler;
  register_handler "mkVar" mkVarHandler;
  register_handler "mkAbs" mkAbsHandler;
  register_handler "closeAbs" closeAbsHandler;
  register_handler "mkApp" mkAppHandler;
  register_handler "closeAppL" closeAppLHandler;
  register_handler "closeAppR" closeAppRHandler;
  register_handler "curTy" curTyHandler;
  register_handler "evalReq" evalReqHandler;
  register_handler "evalResp" evalRespHandler;
  register_handler "closureId" closureIdHandler;
  ec_init ()

let trace_eval_correct trace =
  let rec check = function
    | [] -> true
    | { ev = { op = "evalResp"; args = [ VConst (B res) ] }; _ } :: rest ->
        if not res then false else check rest
    | _ :: rest -> check rest
  in
  check trace

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    [
      "mkCon"#:(record []);
      "mkVar"#:(record [ "x"#:int_ty ]);
      "mkAbs"#:(record [ "ty"#:(mk_p_abstract_ty "stlcTy") ]);
      "closeAbs"#:(record [ "sid"#:int_ty ]);
      "mkApp"#:(record []);
      "closeAppL"#:(record [ "sid"#:int_ty; "ty"#:(mk_p_abstract_ty "stlcTy") ]);
      "closeAppR"#:(record [ "sid"#:int_ty; "ty"#:(mk_p_abstract_ty "stlcTy") ]);
      "curTy"#:(record [ "ty"#:(mk_p_abstract_ty "stlcTy") ]);
      "closureId"#:(record [ "sid"#:int_ty ]);
      "evalReq"#:(record []);
      "evalResp"#:(record [ "res"#:bool_ty ]);
    ]

let genMkAbs ty body = mk_term_gen testCtx "mkAbs" [ mk_value_stlcTy ty ] body
let genMkApp = mk_term_gen testCtx "mkApp" []
let genMkVar x = mk_term_gen testCtx "mkVar" [ mk_value_int x ]
let genMkCon = mk_term_gen testCtx "mkCon" []
let genCloseAbs = mk_term_gen testCtx "closeAbs" [ mk_value_int 0 ]

let genCloseAppL =
  mk_term_gen testCtx "closeAppL" [ mk_value_int 0; mk_value_stlcTy StlcInt ]

let genCloseAppR =
  mk_term_gen testCtx "closeAppR" [ mk_value_int 0; mk_value_stlcTy StlcInt ]

let genEvalReq = mk_term_gen testCtx "evalReq" []
let obsEvalResp e = mk_term_obs_fresh testCtx "evalResp" (fun _ -> e)

let obsCurTy k =
  mk_term_obs_fresh testCtx "curTy" (function
    | [ x ] -> k x
    | _ -> _die [%here])

let testAst () =
  let e =
    StlcApp
      {
        appFun =
          StlcApp
            {
              appFun =
                StlcAbs
                  {
                    absTy = StlcArrow (StlcInt, StlcInt);
                    absBody =
                      StlcAbs
                        {
                          absTy = StlcInt;
                          absBody =
                            StlcApp { appFun = StlcVar 1; appArg = StlcVar 0 };
                        };
                  };
              appArg = StlcAbs { absTy = StlcInt; absBody = StlcVar 0 };
            };
        appArg = StlcConst 3;
      }
  in
  let () = Printf.printf "testAst: %s\n" (layout_stlcTerm e) in
  let () =
    Printf.printf "typeinfer_stlcTerm: %s\n"
      (match typeinfer_stlcTerm [] e with
      | Some ty -> layout_stlcTy ty
      | None -> "None")
  in
  let _ = mstep_stlcTerm e in
  (* let () = Printf.printf "res: %s\n" (layout_stlcTerm res) in *)
  ()

let main =
  (* testAst ();
  let () = _die_with [%here] "done" in *)
  let mkAbsClosure ty e =
    genMkAbs ty (term_concat e (genCloseAbs (obsCurTy (fun _ -> mk_term_tt))))
  in
  let mkAppClosure e1 e2 =
    genMkApp
      (term_concat e1
         (genCloseAppL
            (term_concat e2 (genCloseAppR (obsCurTy (fun _ -> mk_term_tt))))))
  in
  let mkVarClosure i = genMkVar i (obsCurTy (fun _ -> mk_term_tt)) in
  let mkConstClosure = genMkCon (obsCurTy (fun _ -> mk_term_tt)) in
  let e =
    mkAppClosure
      (mkAppClosure
         (mkAbsClosure
            (StlcArrow (StlcInt, StlcInt))
            (mkAbsClosure StlcInt
               (mkAppClosure (mkVarClosure 1) (mkVarClosure 0))))
         (mkAbsClosure StlcInt (mkVarClosure 0)))
      mkConstClosure
  in
  term_concat e (genEvalReq (obsEvalResp mk_term_tt))

type stlc_bench_config = { depthBound : int; constRange : int }

let ty_gen depth =
  let open QCheck.Gen in
  fix
    (fun self depth ->
      if depth <= 0 then pure StlcInt
      else
        frequency
          [
            (1, pure StlcInt);
            ( 1,
              map2
                (fun ty1 ty2 -> StlcArrow (ty1, ty2))
                (self (depth - 1))
                (self (depth - 1)) );
          ])
    depth

let stlc_gen { depthBound; constRange } =
  let open QCheck.Gen in
  let genvar ctx ty =
    let is =
      List.filter_map (fun (i, x) -> if equal_stlcTy x ty then Some i else None)
      @@ List.mapi (fun i x -> (i, x)) ctx
    in
    if List.length is == 0 then None
    else Some (map (fun i -> StlcVar i) (oneofl is))
  in
  let genconst = map (fun i -> StlcConst i) (int_range 0 (constRange - 1)) in
  let rec gen_base ctx ty =
    match ty with
    | StlcInt -> (
        match genvar ctx ty with
        | None -> genconst
        | Some g -> frequency [ (1, g); (1, genconst) ])
    | StlcArrow (ty1, ty2) ->
        map
          (fun e2 -> StlcAbs { absTy = ty1; absBody = e2 })
          (gen_base (ty1 :: ctx) ty2)
  in
  let rec aux (ctx : stlcTy list) numApp ty =
    if numApp <= 0 then gen_base ctx ty
    else
      match ty with
      | StlcInt -> (
          match genvar ctx ty with
          | None -> frequency [ (1, genconst); (1, aux_app ctx numApp ty) ]
          | Some g ->
              frequency [ (1, g); (1, genconst); (1, aux_app ctx numApp ty) ])
      | StlcArrow (ty1, ty2) -> (
          let abs =
            map
              (fun e2 -> StlcAbs { absTy = ty1; absBody = e2 })
              (aux (ty1 :: ctx) (numApp - 1) ty2)
          in
          let app = aux_app ctx numApp ty in
          match genvar ctx ty with
          | None -> frequency [ (1, app); (1, abs) ]
          | Some g -> frequency [ (1, g); (1, app); (1, abs) ])
  and aux_app ctx numApp ty =
    ty_gen 3 >>= fun t2 ->
    let t1 = StlcArrow (t2, ty) in
    aux ctx (numApp - 1) t1 >>= fun e1 ->
    aux ctx (numApp - 1) t2 >>= fun e2 ->
    pure (StlcApp { appFun = e1; appArg = e2 })
  in
  ty_gen 3 >>= fun ty -> aux [] depthBound ty

let exec_stlc e =
  let rec aux e =
    match e with
    | StlcVar x -> send ("mkVar", [ mk_value_int x ])
    | StlcConst x -> send ("mkCon", [ mk_value_int x ])
    | StlcAbs { absTy; absBody } ->
        send ("mkAbs", [ mk_value_stlcTy absTy ]);
        aux absBody;
        send ("closeAbs", [])
    | StlcApp { appFun; appArg } ->
        send ("mkApp", []);
        aux appFun;
        send ("closeAppL", []);
        aux appArg;
        send ("closeAppR", [])
  in
  aux e;
  send ("evalReq", []);
  Effect.perform End

let random_wt_stlc_term { depthBound; constRange } num =
  let res = QCheck.Gen.generate ~n:num (stlc_gen { depthBound; constRange }) in
  (* let () =
    List.iter
      (fun e ->
        let _ = typeinfer_stlcTerm [] e in
        ())
      res
  in *)
  res

let _store = ref []

let _next conf =
  match !_store with
  | [] -> (
      let res = QCheck.Gen.generate ~n:10000 (stlc_gen conf) in
      match res with
      | [] -> _die_with [%here] "never"
      | e :: es ->
          _store := es;
          e)
  | e :: es ->
      _store := es;
      e

let randomTest conf =
  let e = _next conf in
  let () = Pp.printf "@{<yellow>randomTest@}: %s\n" (layout_stlcTerm e) in
  exec_stlc e
