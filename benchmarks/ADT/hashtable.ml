open Interpreter
open Language
open Zdatatype

module Hashtable = struct
  let init t = t := Hashtbl.create ~random:false 42
  let add t (k : int) (v : int) = Hashtbl.replace !t k v
  let find t (k : int) : int option = Hashtbl.find_opt !t k
  let remove t (k : int) = Hashtbl.remove !t k
  let clear t = Hashtbl.clear !t
  let find_all t (k : int) = Hashtbl.find_all !t k
  let replace t (k : int) (v : int) = Hashtbl.replace !t k v
  let mem t (k : int) = Hashtbl.mem !t k
  let length t = Hashtbl.length !t
end

open Hashtable

let _tbl : (int, int) Hashtbl.t ref = ref (Hashtbl.create ~random:false 42)

let initTblReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  init _tbl

let addReqHandler (msg : msg) =
  let k, v =
    match msg.ev.args with
    | [ VConst (I k); VConst (I v) ] -> (k, v)
    | _ -> _die [%here]
  in
  add _tbl k v

let findReqHandler (msg : msg) =
  let k = match msg.ev.args with [ VConst (I k) ] -> k | _ -> _die [%here] in
  let v = find _tbl k in
  match v with
  | None -> send ("findResp", [ mk_value_int k; mk_value_int (-127) ])
  | Some v -> send ("findResp", [ mk_value_int k; mk_value_int v ])

let removeReqHandler (msg : msg) =
  let k = match msg.ev.args with [ VConst (I k) ] -> k | _ -> _die [%here] in
  remove _tbl k

let clearReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  clear _tbl

let findAllReqHandler (msg : msg) =
  let k = match msg.ev.args with [ VConst (I k) ] -> k | _ -> _die [%here] in
  let vs = find_all _tbl k in
  let value_list = List.map mk_value_int vs in
  send ("findAllResp", value_list)

let replaceReqHandler (msg : msg) =
  let k, v =
    match msg.ev.args with
    | [ VConst (I k); VConst (I v) ] -> (k, v)
    | _ -> _die [%here]
  in
  replace _tbl k v

let memReqHandler (msg : msg) =
  let k = match msg.ev.args with [ VConst (I k) ] -> k | _ -> _die [%here] in
  let exists = mem _tbl k in
  send ("memResp", [ mk_value_bool exists ])

let lengthReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  let l = length _tbl in
  send ("lengthResp", [ mk_value_int l ])

let findRespHandler (_ : msg) = ()
let findAllRespHandler (_ : msg) = ()
let memRespHandler (_ : msg) = ()
let lengthRespHandler (_ : msg) = ()

let init () =
  register_handler "initTblReq" initTblReqHandler;
  register_handler "addReq" addReqHandler;
  register_handler "findReq" findReqHandler;
  register_handler "removeReq" removeReqHandler;
  register_handler "clearReq" clearReqHandler;
  register_handler "findAllReq" findAllReqHandler;
  register_handler "replaceReq" replaceReqHandler;
  register_handler "memReq" memReqHandler;
  register_handler "lengthReq" lengthReqHandler;

  register_handler "findResp" findRespHandler;
  register_handler "findAllResp" findAllRespHandler;
  register_handler "memResp" memRespHandler;
  register_handler "lengthResp" lengthRespHandler;

  init _tbl

let check_membership_hashtable trace =
  let rec check (added_values, found_values) = function
    | [] -> IntSet.subset added_values found_values
    | { ev = { op = "initTblReq" | "clearReq"; args = []; _ }; _ } :: rest ->
        check (IntSet.empty, IntSet.empty) rest
    | {
        ev =
          {
            op = "addReq" | "replaceReq";
            args = [ VConst (I _key); VConst (I v) ];
          };
        _;
      }
      :: rest ->
        check (IntSet.add v added_values, found_values) rest
    | { ev = { op = "findResp"; args = [ VConst (I v) ] }; _ } :: rest ->
        if v = -127 then check (added_values, found_values) rest
        else check (added_values, IntSet.add v found_values) rest
    | _ :: rest -> check (added_values, found_values) rest
  in
  check (IntSet.empty, IntSet.empty) trace
