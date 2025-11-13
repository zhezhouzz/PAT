open Interpreter
open Language
open Zdatatype

module Set = struct
  let _max_size = 1
  let init s = s := []
  let isMember s (x : int) = List.mem x !s

  let add s (x : int) =
    if isMember s x then ()
    else if List.length !s >= _max_size then
      match !s with [] -> _die [%here] | _ :: s' -> s := s'
    else s := x :: !s

  let remove s x : int option =
    if List.exists (fun y -> y == x) !s then (
      let s' = List.filter (fun y -> y != x) !s in
      s := s';
      Some x)
    else None

  let isEmpty s = match !s with [] -> true | _ -> false
end

open Set

let _set : int list ref = ref []

let isMemberRespHandler (msg : msg) =
  let x = match msg.ev.args with [ VConst (I x) ] -> x | _ -> _die [%here] in
  let is_member = isMember _set x in
  send ("isMemberResp", [ mk_value_bool is_member ])

let initSetHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  init _set

let insertHandler (msg : msg) =
  let x = match msg.ev.args with [ VConst (I x) ] -> x | _ -> _die [%here] in
  add _set x

let deleteReqHandler (msg : msg) =
  let x = match msg.ev.args with [ VConst (I x) ] -> x | _ -> _die [%here] in
  let res = remove _set x in
  match res with
  | None -> send ("deleteResp", [ mk_value_bool false ])
  | Some _ -> send ("deleteResp", [ mk_value_bool true ])

let isEmptyReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  let is_empty = isEmpty _set in
  send ("isEmptyResp", [ mk_value_bool is_empty ])

let initSetRespHandler (_ : msg) = ()
let addRespHandler (_ : msg) = ()
let deleteRespHandler (_ : msg) = ()
let isEmptyRespHandler (_ : msg) = ()
let isMemberReqHandler (_ : msg) = ()
let sizeHandler (_ : msg) = ()

let init () =
  register_handler "size" sizeHandler;
  register_handler "initSet" initSetHandler;
  register_handler "insert" insertHandler;
  register_handler "deleteReq" deleteReqHandler;
  register_handler "deleteResp" deleteRespHandler;
  register_handler "isEmptyReq" isEmptyReqHandler;
  register_handler "isMemberReq" isMemberReqHandler;
  register_handler "isEmptyResp" isEmptyRespHandler;
  register_handler "isMemberResp" isMemberRespHandler;
  init _set

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    [
      "initSet"#:(record []);
      "insert"#:(record [ "x"#:int_ty ]);
      "delete"#:(record [ "x"#:int_ty ]);
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
  let rec check set = function
    | [] -> true
    | { ev = { op = "initSet"; args = [] }; _ } :: rest ->
        check IntSet.empty rest
    | { ev = { op = "insert"; args = [ VConst (I x) ] }; _ } :: rest ->
        check (IntSet.add x set) rest
    | { ev = { op = "deleteReq"; args = [ VConst (I x) ] }; _ }
      :: { ev = { op = "deleteResp"; args = [ VConst (B res) ] }; _ }
      :: rest ->
        let ex = IntSet.mem x set in
        if res != ex then false else check (IntSet.remove x set) rest
    | _ :: rest -> check set rest
  in
  check IntSet.empty trace

let main =
  mk_term_assume_fresh_neq int_ty [] (fun x ->
      mk_term_assume_fresh_neq int_ty [ x ] (fun y ->
          mk_term_assume_fresh_neq int_ty [ x; y ] (fun z ->
              mk_term_assume_fresh_neq int_ty [ x; y; z ] (fun w ->
                  gen "initSet" [] @@ obsInitSetResp @@ gen "insert" [ x ]
                  @@ obsAddResp @@ gen "insert" [ y ] @@ obsAddResp
                  @@ gen "insert" [ z ] @@ obsAddResp @@ gen "insert" [ w ]
                  @@ obsAddResp @@ gen "delete" [] @@ obsRemoveResp
                  @@ gen "delete" [] @@ obsRemoveResp @@ gen "delete" []
                  @@ obsRemoveResp @@ gen "delete" [] @@ obsRemoveResp
                  @@ gen "isEmptyReq" [] @@ obsIsEmptyResp mk_term_tt))))

type stack_bench_config = { numElem : int; numOp : int }

let parse_config config =
  let numElem = get_config_value config "numSet" in
  let numOp = get_config_value config "numOp" in
  { numElem; numOp }

let randomTest config =
  let { numElem; numOp } = parse_config config in
  let elems = List.init numElem (fun i -> i + 1) in
  let currentSize = ref 0 in
  let random_isMember () =
    let elem = List.nth elems (Random.int numElem) in
    send ("isMemberReq", [ mk_value_int elem ])
  in
  let random_add () =
    let elem = List.nth elems (Random.int numElem) in
    send ("insert", [ mk_value_int elem ]);
    currentSize := !currentSize + 1
  in
  let random_remove () =
    let elem = List.nth elems (Random.int numElem) in
    send ("deleteReq", [ mk_value_int elem ]);
    currentSize := !currentSize - 1
  in
  let random_isEmpty () = send ("isEmptyReq", []) in
  let random_init () =
    send ("initSet", []);
    currentSize := 0
  in
  let rec genOp restNum =
    if restNum <= 0 then ()
    else
      let () = Pp.printf "@{<yellow>restNum@}: %i\n" restNum in
      (match Random.int 5 with
      | 0 -> random_isEmpty ()
      | 1 -> random_add ()
      | 2 -> random_remove ()
      | 3 -> random_isMember ()
      | _ -> random_init ());
      genOp (restNum - 1)
  in
  let () = genOp numOp in
  let () = Pp.printf "@{<red>End with numOp@}\n%i\n" numOp in
  Effect.perform End

let test_env =
  {
    if_concurrent = false;
    database_ctx = None;
    init_test_env = init;
    default_test_prog = [];
    property = check_membership_set;
    random_test_gen = randomTest;
  }
