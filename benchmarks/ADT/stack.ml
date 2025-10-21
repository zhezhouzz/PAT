open Interpreter
open Language
open Zdatatype

(** Core Implementation *)
module Stack = struct
  let _max_size = 5
  let init s = s := []

  let push s (x : int) =
    if List.length !s >= _max_size then
      match !s with [] -> _die [%here] | _ :: s' -> s := s'
    else s := x :: !s

  let pop s : int option =
    match !s with
    | [] -> None
    | x :: s' ->
        s := s';
        Some x

  let isEmpty s = match !s with [] -> true | _ -> false
end

open Stack

(** Handlers *)

let _stack : int list ref = ref []

let initStackReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  init _stack

let pushReqHandler (msg : msg) =
  let x = match msg.ev.args with [ VConst (I x) ] -> x | _ -> _die [%here] in
  push _stack x

let popReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  let x = pop _stack in
  match x with
  | None -> send ("popResp", [ mk_value_int (-10) ])
  | Some x -> send ("popResp", [ mk_value_int x ])

let isEmptyReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  let is_empty = isEmpty _stack in
  send ("isEmptyResp", [ mk_value_bool is_empty ])

let initStackRespHandler (_ : msg) = ()
let pushRespHandler (_ : msg) = ()
let popRespHandler (_ : msg) = ()
let isEmptyRespHandler (_ : msg) = ()

(** Initialization *)
let init () =
  register_handler "initStackReq" initStackReqHandler;
  register_handler "pushReq" pushReqHandler;
  register_handler "popReq" popReqHandler;
  register_handler "isEmptyReq" isEmptyReqHandler;
  register_handler "popResp" popRespHandler;
  register_handler "isEmptyResp" isEmptyRespHandler;
  init _stack

(* let trace_checker trace =
  let update_first_pushed firstPushed x =
    match firstPushed with None -> Some x | Some _ -> firstPushed
  in
  let update_last_popped _ x = Some x in
  let rec check (firstPushed, lastPopped) = function
    | [] -> true
    | { ev = { op = "initStackReq"; args = [] }; _ } :: rest ->
        check (None, None) rest
    | { ev = { op = "pushReq"; args = [ VConst (I x) ] }; _ } :: rest ->
        check (update_first_pushed firstPushed x, lastPopped) rest
    | { ev = { op = "popResp"; args = [ VConst (I x) ] }; _ } :: rest ->
        check (firstPushed, update_last_popped lastPopped x) rest
    | { ev = { op = "isEmptyResp"; args = [ VConst (B is_empty) ] }; _ } :: rest
      ->
        let res =
          if is_empty then
            match (firstPushed, lastPopped) with
            | Some x, Some y -> x == y
            | None, None -> true
            | _, _ -> false
          else true
        in
        res && check (firstPushed, lastPopped) rest
    | _ :: rest -> check (firstPushed, lastPopped) rest
  in
  check (None, None) trace *)

(** Executable Properties *)
let check_membership_stack trace =
  let rec check (inSet, outSet) = function
    | [] -> true
    | { ev = { op = "initStackReq"; args = [] }; _ } :: rest ->
        check (IntSet.empty, IntSet.empty) rest
    | { ev = { op = "pushReq"; args = [ VConst (I x) ] }; _ } :: rest ->
        check (IntSet.add x inSet, outSet) rest
    | { ev = { op = "popResp"; args = [ VConst (I x) ] }; _ } :: rest ->
        check (inSet, IntSet.add x outSet) rest
    | { ev = { op = "isEmptyResp"; args = [ VConst (B isEmpty) ] }; _ } :: rest
      ->
        if isEmpty then
          if IntSet.subset inSet outSet then check (inSet, outSet) rest
          else false
        else check (inSet, outSet) rest
    | _ :: rest -> check (inSet, outSet) rest
  in
  check (IntSet.empty, IntSet.empty) trace

(* let main =
  mk_term_assume_fresh_neq int_ty [] (fun x ->
      mk_term_assume_fresh_neq int_ty [ x ] (fun y ->
          mk_term_assume_fresh_neq int_ty [ x; y ] (fun z ->
              mk_term_assume_fresh_neq int_ty [ x; y; z ] (fun w ->
                  gen "initStackReq" [] @@ obsInitStackResp
                  @@ gen "pushReq" [ x ] @@ obsPushResp @@ gen "pushReq" [ y ]
                  @@ obsPushResp @@ gen "pushReq" [ z ] @@ obsPushResp
                  @@ gen "pushReq" [ w ] @@ obsPushResp @@ gen "popReq" []
                  @@ obsPopResp @@ gen "popReq" [] @@ obsPopResp
                  @@ gen "popReq" [] @@ obsPopResp @@ gen "popReq" []
                  @@ obsPopResp @@ gen "isEmptyReq" []
                  @@ obsIsEmptyResp mk_term_tt)))) *)

(** QuickCheck Generators*)

type stack_bench_config = { numElem : int; numOp : int }

let randomTest { numElem; numOp } =
  let elems = List.init numElem (fun i -> i + 1) in
  let currentSize = ref 0 in
  let random_push () =
    let elem = List.nth elems (Random.int numElem) in
    send ("pushReq", [ mk_value_int elem ]);
    currentSize := !currentSize + 1
  in
  let random_pop () =
    send ("popReq", []);
    currentSize := !currentSize - 1
  in
  let random_isEmpty () = send ("isEmptyReq", []) in
  let random_init () =
    send ("initStackReq", []);
    currentSize := 0
  in
  let rec genOp restNum =
    if restNum <= 0 then ()
    else
      let () = Pp.printf "@{<yellow>restNum@}: %i\n" restNum in
      (if !currentSize <= 0 then
         match Random.int 3 with
         | 0 -> random_isEmpty ()
         | 1 -> random_push ()
         | _ -> random_init ()
       else
         match Random.int 4 with
         | 0 -> random_isEmpty ()
         | 1 -> random_push ()
         | 2 -> random_pop ()
         | _ -> random_init ());
      genOp (restNum - 1)
  in
  let () = random_init () in
  let () = genOp numOp in
  let () = Pp.printf "@{<red>End with numOp@}\n%i\n" numOp in
  Effect.perform End

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    [
      "initStackReq"#:(record []);
      "pushReq"#:(record [ "x"#:int_ty ]);
      "popReq"#:(record []);
      "isEmptyReq"#:(record []);
      "isEmptyResp"#:(record [ "isEmpty"#:bool_ty ]);
      "initStackResp"#:(record []);
      "popResp"#:(record [ "x"#:int_ty ]);
    ]

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)
let obsInitStackResp e = mk_term_obs_fresh testCtx "initStackResp" (fun _ -> e)
let obsPushResp e = mk_term_obs_fresh testCtx "pushResp" (fun _ -> e)
let obsPopResp e = mk_term_obs_fresh testCtx "popResp" (fun _ -> e)
let obsIsEmptyResp e = mk_term_obs_fresh testCtx "isEmptyResp" (fun _ -> e)

let rec_main =
  let open Nt in
  let recfunc =
    mk_rec mk_term_tt
      (mk_term_assume_fresh_true int_ty (fun y ->
           gen "pushReq" [ y ]
           @@ mk_let_ (mk_rec_app_incr mk_term_tt)
           @@ gen "popReq" [] @@ obsPopResp mk_term_tt))
  in
  recfunc (mk_rec_app_0 (gen "isEmptyReq" [] @@ obsIsEmptyResp mk_term_tt))

(* let rec_main =
  let open Nt in
  mk_term_assume_fresh_geq_zero int_ty (fun x ->
      let recfunc =
        mk_rec mk_term_tt
          (mk_term_assume_fresh_true int_ty (fun y ->
               gen "pushReq" [ y ]
               @@ term_concat (mk_rec_app_incr ())
               @@ gen "popReq" [] @@ obsPopResp mk_term_tt))
      in
      term_concat recfunc
      @@ term_concat (mk_rec_app_0 (VVar x))
      @@ gen "isEmptyReq" [] @@ obsIsEmptyResp mk_term_tt) *)
