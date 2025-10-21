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

let find_stlcTy_opt ctx x = List.nth_opt ctx x
(* let () = Printf.printf "find_stlcTy %d\n" x in *)
(* let () = Printf.printf "ctx: %s\n" (layout_stlcCtx ctx) in *)
(* let () = Printf.printf "ty: %s\n" (layout_stlcTy ty) in *)

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
    | StlcVar x -> (
        if x >= List.length ctx then type_error ()
        else
          match find_stlcTy_opt ctx x with
          | None -> type_error ()
          | Some ty -> Some ty)
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
  | StlcConst x -> StlcConst x
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

let layout_eval_result = function
  | EvalSuccess e -> spf "EvalSuccess %s" (layout_stlcTerm e)
  | EvalError e -> spf "EvalError %s" (layout_stlcTerm e)
  | EvalNormalFrom e -> spf "EvalNormalFrom %s" (layout_stlcTerm e)

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
  let _ty = ref StlcInt
  let _depth = ref 0
  let eCtx = ref []

  let ec_init () =
    _sid := 0;
    _tmp := StlcConst 0;
    _ty := StlcInt;
    _depth := 0;
    eCtx := []

  let putTy ty = _ty := ty
  let putDepth depth = _depth := depth
  let getTy () = !_ty
  let getDepth () = !_depth

  let layout ctx =
    let rec aux = function
      | [] -> "□"
      | MkAbs ty :: ctx -> spf "\\(%s).%s" (layout_stlcTy ty) (aux ctx)
      | MkAppL :: ctx -> spf "(%s □)" (aux ctx)
      | MkAppR e :: ctx -> spf "(%s %s)" (layout_stlcTerm e) (aux ctx)
    in
    aux (List.rev ctx)

  let step e = function
    | [] -> _die_with [%here] "not a well-formed term"
    | MkAbs absTy :: ctx ->
        _tmp := StlcAbs { absTy; absBody = e };
        ctx
    | MkAppL :: ctx -> MkAppR e :: ctx
    | MkAppR e1 :: ctx ->
        _tmp := StlcApp { appFun = e1; appArg = e };
        ctx

  let constHandler (_ : msg) = _tmp := StlcConst (Random.int 5)

  let eCtx2Ctx ctx =
    let rec aux = function
      | [] -> []
      | MkAbs ty :: ctx -> ty :: aux ctx
      | _ :: ctx -> aux ctx
    in
    aux ctx

  let putTyHandler (msg : msg) =
    let ty =
      match msg.ev.args with [ VCStlcTy ty ] -> ty | _ -> _die [%here]
    in
    putTy ty

  let putDepthHandler (msg : msg) =
    let depth =
      match msg.ev.args with [ VConst (I depth) ] -> depth | _ -> _die [%here]
    in
    putDepth depth

  let ayncGetTyHandler (_ : msg) = send ("getTy", [ mk_value_stlcTy !_ty ])
  let ayncGetDepthHandler (_ : msg) = send ("getDepth", [ mk_value_int !_depth ])
  let getTyHandler (_ : msg) = ()
  let getDepthHandler (_ : msg) = ()

  let varHandler (msg : msg) =
    let x =
      match msg.ev.args with VConst (I x) :: _ -> x | _ -> _die [%here]
    in
    _tmp := StlcVar x

  let absHandler (msg : msg) =
    let ty =
      match msg.ev.args with VCStlcTy ty :: _ -> ty | _ -> _die [%here]
    in
    eCtx := MkAbs ty :: !eCtx;
    send ("closureId", [ mk_value_int (next_sid ()) ])

  let endAbsHandler (_ : msg) =
    match !eCtx with
    | [] -> _die_with [%here] "not a well-formed term"
    | _ -> eCtx := step !_tmp !eCtx

  let appHandler (_ : msg) =
    eCtx := MkAppL :: !eCtx;
    send ("closureId", [ mk_value_int (next_sid ()) ])

  let closureIdHandler (_ : msg) = ()

  let appLHandler (_ : msg) =
    match !eCtx with
    | [] -> _die_with [%here] "not a well-formed term"
    | _ -> eCtx := step !_tmp !eCtx

  let appRHandler (_ : msg) =
    match !eCtx with
    | [] -> _die_with [%here] "not a well-formed term"
    | _ -> eCtx := step !_tmp !eCtx

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
  register_handler "const" constHandler;
  register_handler "var" varHandler;
  register_handler "abs" absHandler;
  register_handler "endAbs" endAbsHandler;
  register_handler "app" appHandler;
  register_handler "appL" appLHandler;
  register_handler "appR" appRHandler;
  register_handler "putTy" putTyHandler;
  register_handler "putDepth" putDepthHandler;
  register_handler "ayncGetDepth" ayncGetDepthHandler;
  register_handler "getDepth" getDepthHandler;
  (* register_handler "evalReq" evalReqHandler;
  register_handler "evalResp" evalRespHandler; *)
  register_handler "closureId" closureIdHandler;
  ec_init ()

let trace_eval_correct _ =
  let ty = typeinfer_stlcTerm [] !_tmp in
  let () =
    Printf.printf "typeinfer_stlcTerm: %s\n"
      (match ty with Some ty -> layout_stlcTy ty | None -> "None")
  in
  match ty with
  | None -> true
  | Some _ -> (
      let res = mstep_stlcTerm !_tmp in
      match res with
      | EvalNormalFrom _ -> true
      | EvalError _ -> false
      | EvalSuccess _ -> false)

(* let rec check = function
    | [] -> true
    | { ev = { op = "evalResp"; args = [ VConst (B res) ] }; _ } :: rest ->
        if not res then false else check rest
    | _ :: rest -> check rest
  in
  check trace *)

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    [
      "const"#:(record []);
      "var"#:(record [ "x"#:int_ty; "ty"#:(mk_p_abstract_ty "stlcTy") ]);
      "abs"#:(record [ "ty"#:(mk_p_abstract_ty "stlcTy") ]);
      "endAbs"#:(record [ "sid"#:int_ty; "ty"#:(mk_p_abstract_ty "stlcTy") ]);
      "app"#:(record []);
      "appL"#:(record [ "sid"#:int_ty ]);
      "appR"#:(record [ "sid"#:int_ty; "ty"#:(mk_p_abstract_ty "stlcTy") ]);
      (* "putTy"#:(record [ "ty"#:(mk_p_abstract_ty "stlcTy") ]); *)
      "putDepth"#:(record [ "depth"#:int_ty ]);
      "ayncGetDepth"#:(record []);
      "getDepth"#:(record [ "depth"#:int_ty ]);
      "closureId"#:(record [ "sid"#:int_ty ]);
      (* "evalReq"#:(record []);
      "evalResp"#:(record [ "res"#:bool_ty ]); *)
    ]

let genPutDepth v = mk_term_gen testCtx "putDepth" [ v ]

let obsGetDepth k =
  mk_term_gen testCtx "ayncGetDepth" []
  @@ mk_term_obs_fresh testCtx "getDepth" (function
       | [ x ] -> k x
       | _ -> _die [%here])

let genAbs t1 t2 body =
  obsGetDepth (fun d ->
      mk_term_gen testCtx "abs" [ mk_value_stlcTy t1 ]
      @@ mk_term_obs_fresh testCtx "closureId" (function
           | [ x ] ->
               term_concat
                 (mk_incr (VVar d) (fun d' -> genPutDepth (VVar d') body))
                 (mk_term_gen testCtx "endAbs"
                    [ VVar x; mk_value_stlcTy t2 ]
                    mk_term_tt)
           | _ -> _die [%here]))

let genApp t2 e1 e2 =
  mk_term_gen testCtx "app" []
  @@ mk_term_obs_fresh testCtx "closureId" (function
       | [ x ] ->
           term_concat e1
           @@ mk_term_gen testCtx "appL" [ VVar x ]
           @@ term_concat e2
           @@ mk_term_gen testCtx "appR"
                [ VVar x; mk_value_stlcTy t2 ]
                mk_term_tt
       | _ -> _die [%here])

let genVar t2 v = mk_term_gen testCtx "var" [ v; mk_value_stlcTy t2 ] mk_term_tt
let genConst = mk_term_gen testCtx "const" [] mk_term_tt
let _app e1 e2 = StlcApp { appFun = e1; appArg = e2 }
let _abs ty e = StlcAbs { absTy = ty; absBody = e }

let testAst () =
  let f1 = _abs (StlcArrow (StlcInt, StlcInt)) (_app (StlcVar 0) (StlcVar 1)) in
  let f2 = _abs StlcInt (_app f1 (_abs StlcInt (StlcVar 0))) in
  let e = _app f2 (StlcConst 3) in
  let () = Printf.printf "testAst: %s\n" (layout_stlcTerm e) in
  let () =
    Printf.printf "typeinfer_stlcTerm: %s\n"
      (match typeinfer_stlcTerm [] e with
      | Some ty -> layout_stlcTy ty
      | None -> "None")
  in
  let res = mstep_stlcTerm e in
  let () = Printf.printf "res: %s\n" (layout_eval_result res) in
  e

let main1 =
  genApp StlcInt
    (genAbs StlcInt StlcInt (genVar StlcInt (mk_value_int 0)))
    genConst

let main2 =
  let recursion =
    genApp StlcInt
      (genAbs
         (StlcArrow (StlcInt, StlcInt))
         StlcInt
         (genApp StlcInt
            (genVar StlcInt (mk_value_int 0))
            (genVar StlcInt (mk_value_int 1))))
      (genAbs StlcInt StlcInt (genVar StlcInt (mk_value_int 0)))
  in
  genApp StlcInt (genAbs StlcInt StlcInt recursion) genConst

type stlc_bench_config = { numApp : int; tyDepthBound : int; constRange : int }

let main_rec =
  (* let branch1 =
    obsGetDepth (fun d ->
        mk_term_assume_fresh_geq_zero_lt_x Nt.int_ty d (fun z ->
            genVar StlcInt (VVar z)))
  in *)
  let branch1 =
    obsGetDepth (fun d -> mk_minus (VVar d) (fun y -> genVar StlcInt (VVar y)))
  in
  let branch2 =
    genApp StlcInt
      (genAbs
         (StlcArrow (StlcInt, StlcInt))
         StlcInt
         (genApp StlcInt
            (genVar StlcInt (mk_value_int 0))
            (mk_rec_app_incr mk_term_tt)))
      (genAbs StlcInt StlcInt (genVar StlcInt (mk_value_int 0)))
  in
  mk_rec branch1 branch2
  @@ genApp StlcInt (genAbs StlcInt StlcInt (mk_rec_app_0 mk_term_tt)) genConst

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

let stlc_gen { numApp; tyDepthBound; constRange } =
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
    ty_gen tyDepthBound >>= fun t2 ->
    let t1 = StlcArrow (t2, ty) in
    aux ctx (numApp - 1) t1 >>= fun e1 ->
    aux ctx (numApp - 1) t2 >>= fun e2 ->
    pure (StlcApp { appFun = e1; appArg = e2 })
  in
  ty_gen tyDepthBound >>= fun ty -> aux [] numApp ty

let exec_stlc e =
  let rec aux e =
    match e with
    | StlcVar x -> send ("var", [ mk_value_int x ])
    | StlcConst x -> send ("const", [ mk_value_int x ])
    | StlcAbs { absTy; absBody } ->
        send ("abs", [ mk_value_stlcTy absTy ]);
        aux absBody;
        send ("endAbs", [])
    | StlcApp { appFun; appArg } ->
        send ("app", []);
        aux appFun;
        send ("appL", []);
        aux appArg;
        send ("appR", [])
  in
  aux e;
  send ("evalReq", []);
  Effect.perform End

(* let random_wt_stlc_term { depthBound; constRange } num =
  let res = QCheck.Gen.generate ~n:num (stlc_gen { depthBound; constRange }) in
  res

let dummy_random_wt_stlc_term _ num = List.init num (fun _ -> testAst ()) *)
let _store = ref []

let _next conf =
  match !_store with
  | [] -> (
      let res = QCheck.Gen.generate ~n:10000 (stlc_gen conf) in
      (* let res = List.init 10000 (fun _ -> testAst ()) in *)
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
