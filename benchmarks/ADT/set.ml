open Interpreter
open Language
open Zdatatype

module Set = struct
  let _max_size = 3
  let init s = s := []
  let isMember s (x : int) = List.mem x !s

  let add s (x : int) =
    if isMember s x then ()
    else if List.length !s >= _max_size then
      match !s with [] -> _die [%here] | _ :: s' -> s := s'
    else s := x :: !s

  let remove s : int option =
    match !s with
    | [] -> None
    | x :: s' ->
        s := s';
        Some x

  let isEmpty s = match !s with [] -> true | _ -> false
end

open Set

let _set : int list ref = ref []

let isMemberRespHandler (msg : msg) =
  let x = match msg.ev.args with [ VConst (I x) ] -> x | _ -> _die [%here] in
  let is_member = isMember _set x in
  send ("isMemberResp", [ mk_value_bool is_member ])

let initSetReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  init _set;
  send ("initSetResp", [])

let addReqHandler (msg : msg) =
  let x = match msg.ev.args with [ VConst (I x) ] -> x | _ -> _die [%here] in
  add _set x;
  send ("addResp", [])

let removeReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  let x = remove _set in
  match x with
  | None -> send ("removeResp", [ mk_value_int (-10) ])
  | Some x -> send ("removeResp", [ mk_value_int x ])

let isEmptyReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  let is_empty = isEmpty _set in
  send ("isEmptyResp", [ mk_value_bool is_empty ])

let initSetRespHandler (_ : msg) = ()
let addRespHandler (_ : msg) = ()
let removeRespHandler (_ : msg) = ()
let isEmptyRespHandler (_ : msg) = ()
let isMemberReqHandler (_ : msg) = ()

let init () =
  register_handler "initSetReq" initSetReqHandler;
  register_handler "addReq" addReqHandler;
  register_handler "removeReq" removeReqHandler;
  register_handler "isEmptyReq" isEmptyReqHandler;
  register_handler "isMemberReq" isMemberReqHandler;
  register_handler "initSetResp" initSetRespHandler;
  register_handler "addResp" addRespHandler;
  register_handler "removeResp" removeRespHandler;
  register_handler "isEmptyResp" isEmptyRespHandler;
  register_handler "isMemberResp" isMemberRespHandler;
  init _set

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    [
      "initSetReq"#:(record []);
      "addReq"#:(record [ "x"#:int_ty ]);
      "removeReq"#:(record []);
      "isEmptyReq"#:(record []);
      "isMemberReq"#:(record [ "x"#:int_ty ]);
      "initSetResp"#:(record []);
      "addResp"#:(record []);
      "removeResp"#:(record [ "x"#:int_ty ]);
      "isEmptyResp"#:(record [ "isEmpty"#:bool_ty ]);
      "isMemberResp"#:(record [ "isMember"#:bool_ty ]);
    ]

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)
let obsInitSetResp e = mk_term_obs_fresh testCtx "initSetResp" (fun _ -> e)
let obsAddResp e = mk_term_obs_fresh testCtx "addResp" (fun _ -> e)
let obsRemoveResp e = mk_term_obs_fresh testCtx "removeResp" (fun _ -> e)
let obsIsEmptyResp e = mk_term_obs_fresh testCtx "isEmptyResp" (fun _ -> e)
let obsIsMemberResp e = mk_term_obs_fresh testCtx "isMemberResp" (fun _ -> e)

let check_membership_set trace =
  let rec check (inSet, outSet) = function
    | [] -> true
    | { ev = { op = "initSetReq"; args = [] }; _ } :: rest ->
        check (IntSet.empty, IntSet.empty) rest
    | { ev = { op = "addReq"; args = [ VConst (I x) ] }; _ } :: rest ->
        check (IntSet.add x inSet, outSet) rest
    | { ev = { op = "removeResp"; args = [ VConst (I x) ] }; _ } :: rest ->
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
                  gen "initSetReq" [] @@ obsInitSetResp @@ gen "addReq" [ x ]
                  @@ obsAddResp @@ gen "addReq" [ y ] @@ obsAddResp
                  @@ gen "addReq" [ z ] @@ obsAddResp @@ gen "addReq" [ w ]
                  @@ obsAddResp @@ gen "removeReq" [] @@ obsRemoveResp
                  @@ gen "removeReq" [] @@ obsRemoveResp @@ gen "removeReq" []
                  @@ obsRemoveResp @@ gen "removeReq" [] @@ obsRemoveResp
                  @@ gen "isEmptyReq" [] @@ obsIsEmptyResp mk_term_tt))))

type stack_bench_config = { numElem : int; numOp : int }

let randomTest { numElem; numOp } =
  let elems = List.init numElem (fun i -> i + 1) in
  let currentSize = ref 0 in
  let random_isMember () =
    let elem = List.nth elems (Random.int numElem) in
    send ("isMemberReq", [ mk_value_int elem ])
  in
  let random_add () =
    let elem = List.nth elems (Random.int numElem) in
    send ("addReq", [ mk_value_int elem ]);
    currentSize := !currentSize + 1
  in
  let random_remove () =
    send ("removeReq", []);
    currentSize := !currentSize - 1
  in
  let random_isEmpty () = send ("isEmptyReq", []) in
  let random_init () =
    send ("initSetReq", []);
    currentSize := 0
  in
  let rec genOp restNum =
    if restNum <= 0 then ()
    else
      let () = Pp.printf "@{<yellow>restNum@}: %i\n" restNum in
      (if !currentSize <= 0 then
         match Random.int 3 with
         | 0 -> random_isEmpty ()
         | 1 -> random_add ()
         | _ -> random_init ()
       else
         match Random.int 5 with
         | 0 -> random_isEmpty ()
         | 1 -> random_add ()
         | 2 -> random_remove ()
         | 3 -> random_isMember ()
         | _ -> random_init ());
      genOp (restNum - 1)
  in
  let () = random_init () in
  let () = genOp numOp in
  let () = Pp.printf "@{<red>End with numOp@}\n%i\n" numOp in
  Effect.perform End
