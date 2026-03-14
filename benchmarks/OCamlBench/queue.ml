open Interpreter
open Language
open Zdatatype

module Queue = struct
  let _max_size = 3
  let init q = q := []

  let dequeue q : int option =
    match List.last_destruct_opt !q with
    | Some (q', x) ->
        q := q';
        Some x
    | None -> None

  let rec enqueue q (x : int) =
    if List.length !q >= _max_size then (
      match !q with
      | [] -> _die [%here]
      | _ :: s ->
          q := s;
          enqueue q x)
    else q := x :: !q

  let isEmpty q = match !q with [] -> true | _ -> false
end

open Queue

let _queue : int list ref = ref []

let initQueueReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  init _queue;
  send ("initQueueResp", [])

let enqueueReqHandler (msg : msg) =
  let x = match msg.ev.args with [ VConst (I x) ] -> x | _ -> _die [%here] in
  enqueue _queue x;
  send ("enqueueResp", [])

let dequeueReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  let x = dequeue _queue in
  match x with
  | None -> send ("dequeueResp", [ mk_value_int (-10) ])
  | Some x -> send ("dequeueResp", [ mk_value_int x ])

let isEmptyReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  let is_empty = isEmpty _queue in
  send ("isEmptyResp", [ mk_value_bool is_empty ])

let initQueueRespHandler (_ : msg) = ()
let enqueueRespHandler (_ : msg) = ()
let dequeueRespHandler (_ : msg) = ()
let isEmptyRespHandler (_ : msg) = ()

let init () =
  register_handler "initQueueReq" initQueueReqHandler;
  register_handler "enqueueReq" enqueueReqHandler;
  register_handler "dequeueReq" dequeueReqHandler;
  register_handler "isEmptyReq" isEmptyReqHandler;
  register_handler "initQueueResp" initQueueRespHandler;
  register_handler "enqueueResp" enqueueRespHandler;
  register_handler "dequeueResp" dequeueRespHandler;
  register_handler "isEmptyResp" isEmptyRespHandler;
  init _queue

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    [
      "initQueueReq"#:(record []);
      "enqueueReq"#:(record [ "x"#:int_ty ]);
      "dequeueReq"#:(record []);
      "isEmptyReq"#:(record []);
      "initQueueResp"#:(record []);
      "enqueueResp"#:(record []);
      "dequeueResp"#:(record [ "x"#:int_ty ]);
      "isEmptyResp"#:(record [ "isEmpty"#:bool_ty ]);
    ]

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)
let obsInitQueueResp e = mk_term_obs_fresh testCtx "initQueueResp" (fun _ -> e)
let obsEequeueResp e = mk_term_obs_fresh testCtx "enqueueResp" (fun _ -> e)
let obsDequeueResp e = mk_term_obs_fresh testCtx "dequeueResp" (fun _ -> e)
let obsIsEmptyResp e = mk_term_obs_fresh testCtx "isEmptyResp" (fun _ -> e)

let check_FIFO_queue trace =
  let updateFirstInQueue x = function None -> Some x | Some y -> Some y in
  let rec check firstInQueue = function
    | [] -> true
    | { ev = { op = "initQueueReq"; args = [] }; _ } :: rest -> check None rest
    | { ev = { op = "enqueueReq"; args = [ VConst (I x) ] }; _ } :: rest ->
        check (updateFirstInQueue x firstInQueue) rest
    | { ev = { op = "dequeueResp"; args = [ VConst (I x) ] }; _ } :: rest -> (
        match firstInQueue with
        | None -> check None rest
        | Some y -> if x == y then check None rest else false)
    | _ :: rest -> check firstInQueue rest
  in
  check None trace

let check_membership_queue trace =
  let rec check (inSet, outSet) = function
    | [] -> true
    | { ev = { op = "initQueueReq"; args = [] }; _ } :: rest ->
        check (IntSet.empty, IntSet.empty) rest
    | { ev = { op = "enqueueReq"; args = [ VConst (I x) ] }; _ } :: rest ->
        check (IntSet.add x inSet, outSet) rest
    | { ev = { op = "dequeueResp"; args = [ VConst (I x) ] }; _ } :: rest ->
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

let main =
  mk_term_assume_fresh_neq int_ty [] (fun x ->
      mk_term_assume_fresh_neq int_ty [ x ] (fun y ->
          mk_term_assume_fresh_neq int_ty [ x; y ] (fun z ->
              mk_term_assume_fresh_neq int_ty [ x; y; z ] (fun w ->
                  gen "enqueueReq" [ x ] @@ obsEequeueResp
                  @@ gen "enqueueReq" [ y ] @@ obsEequeueResp
                  @@ gen "enqueueReq" [ z ] @@ obsEequeueResp
                  @@ gen "enqueueReq" [ w ] @@ obsEequeueResp
                  @@ gen "dequeueReq" [] @@ obsDequeueResp
                  @@ gen "dequeueReq" [] @@ obsDequeueResp
                  @@ gen "dequeueReq" [] @@ obsDequeueResp
                  @@ gen "dequeueReq" [] @@ obsDequeueResp
                  @@ gen "isEmptyReq" [] @@ obsIsEmptyResp mk_term_tt))))

type queue_bench_config = { numElem : int; numOp : int }

let randomTest { numElem; numOp } =
  let elems = List.init numElem (fun i -> i + 1) in
  let currentSize = ref 0 in
  let random_enqueue () =
    let elem = List.nth elems (Random.int numElem) in
    send ("enqueueReq", [ mk_value_int elem ]);
    currentSize := !currentSize + 1
  in
  let random_dequeue () =
    send ("dequeueReq", []);
    currentSize := !currentSize - 1
  in
  let random_isEmpty () = send ("isEmptyReq", []) in
  let random_init () =
    send ("initQueueReq", []);
    currentSize := 0
  in
  let rec genOp restNum =
    if restNum <= 0 then ()
    else
      let () = Pp.printf "@{<yellow>restNum@}: %i\n" restNum in
      (if !currentSize <= 0 then
         match Random.int 3 with
         | 0 -> random_isEmpty ()
         | 1 -> random_enqueue ()
         | _ -> random_init ()
       else
         match Random.int 4 with
         | 0 -> random_isEmpty ()
         | 1 -> random_enqueue ()
         | 2 -> random_dequeue ()
         | _ -> random_init ());
      genOp (restNum - 1)
  in
  let () = genOp numOp in
  let () = Pp.printf "@{<red>End with numOp@}\n%i\n" numOp in
  Effect.perform End
