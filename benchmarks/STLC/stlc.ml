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
  (* let () =
    Printf.printf "typeinfer_stlcTerm %s |- %s\n" (layout_stlcCtx ctx)
      (layout_stlcTerm e)
  in *)
  match e with
  | StlcVar x -> find_stlcTy ctx x
  | StlcConst _ -> StlcInt
  | StlcAbs { absTy; absBody } ->
      let ty = typeinfer_stlcTerm (absTy :: ctx) absBody in
      StlcArrow (absTy, ty)
  | StlcApp { appFun; appArg } -> (
      let ty1 = typeinfer_stlcTerm ctx appFun in
      let ty2 = typeinfer_stlcTerm ctx appArg in
      match ty1 with
      | StlcArrow (ty11, ty12) ->
          if equal_stlcTy ty11 ty2 then ty12 else _die_with [%here] "type error"
      | _ -> _die_with [%here] "type error")

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

let rec step_stlcTerm = function
  | StlcApp { appFun = StlcAbs { absBody; absTy }; appArg } -> (
      match step_stlcTerm appArg with
      | None ->
          (* let e =
            shift_stlcTerm 0 (-1)
            @@ subst_stlcTerm (0, shift_stlcTerm 0 1 appArg) absBody
          in *)
          let e = shift_stlcTerm 0 (-1) (wrong_subst (0, appArg) absBody) in
          (* let e = subst_stlcTerm (0, appArg) (shift_stlcTerm 0 (-1) absBody) in *)
          Some e
      | Some e ->
          Some (StlcApp { appFun = StlcAbs { absBody; absTy }; appArg = e }))
  | StlcApp { appFun; appArg } -> (
      match step_stlcTerm appFun with
      | None -> _die_with [%here] "not a beta-reducible term"
      | Some e -> Some (StlcApp { appFun = e; appArg }))
  | _ -> None

let rec mstep_stlcTerm e =
  let () = Printf.printf "mstep_stlcTerm: %s\n" (layout_stlcTerm e) in
  match step_stlcTerm e with None -> e | Some e -> mstep_stlcTerm e

open Interpreter

module EvaluationCtx = struct
  type t = MkAbs of stlcTy | MkAppL | MkAppR of stlcTerm

  let layout ctx =
    let rec aux = function
      | [] -> "□"
      | MkAbs ty :: ctx -> spf "\\(%s).%s" (layout_stlcTy ty) (aux ctx)
      | MkAppL :: ctx -> spf "(%s □)" (aux ctx)
      | MkAppR e :: ctx -> spf "(%s %s)" (layout_stlcTerm e) (aux ctx)
    in
    aux (List.rev ctx)

  let _tmp = ref (StlcConst 0)
  let eCtx = ref []

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
    eCtx := MkAbs ty :: !eCtx

  let closeAbsHandler (_ : msg) =
    match !eCtx with
    | [] -> _die_with [%here] "not a well-formed term"
    | _ ->
        eCtx := step !_tmp !eCtx;
        sendCurTy (typeinfer_stlcTerm (eCtx2Ctx !eCtx) !_tmp)

  let mkAppHandler (_ : msg) = eCtx := MkAppL :: !eCtx

  let closeAppLHandler (_ : msg) =
    match !eCtx with
    | [] -> _die_with [%here] "not a well-formed term"
    | _ -> eCtx := step !_tmp !eCtx

  let closeAppRHandler (_ : msg) =
    match !eCtx with
    | [] -> _die_with [%here] "not a well-formed term"
    | _ ->
        eCtx := step !_tmp !eCtx;
        sendCurTy (typeinfer_stlcTerm (eCtx2Ctx !eCtx) !_tmp)

  let curTyHandler (_ : msg) = ()
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
  register_handler "curTy" curTyHandler

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    [
      "mkCon"#:(record []);
      "mkVar"#:(record [ "x"#:int_ty ]);
      "mkAbs"#:(record [ "ty"#:(mk_p_abstract_ty "stlcTy") ]);
      "closeAbs"#:(record []);
      "mkApp"#:(record []);
      "closeAppL"#:(record []);
      "closeAppR"#:(record []);
      "curTy"#:(record [ "ty"#:(mk_p_abstract_ty "stlcTy") ]);
    ]

let genMkAbs ty body = mk_term_gen testCtx "mkAbs" [ mk_value_stlcTy ty ] body
let genMkApp = mk_term_gen testCtx "mkApp" []
let genMkVar x = mk_term_gen testCtx "mkVar" [ mk_value_int x ]
let genMkCon = mk_term_gen testCtx "mkCon" []
let genCloseAbs = mk_term_gen testCtx "closeAbs" []
let genCloseAppL = mk_term_gen testCtx "closeAppL" []
let genCloseAppR = mk_term_gen testCtx "closeAppR" []

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
      (layout_stlcTy (typeinfer_stlcTerm [] e))
  in
  let res = mstep_stlcTerm e in
  let () = Printf.printf "res: %s\n" (layout_stlcTerm res) in
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
  mkAppClosure
    (mkAppClosure
       (mkAbsClosure
          (StlcArrow (StlcInt, StlcInt))
          (mkAbsClosure StlcInt
             (mkAppClosure (mkVarClosure 1) (mkVarClosure 0))))
       (mkAbsClosure StlcInt (mkVarClosure 0)))
    mkConstClosure
