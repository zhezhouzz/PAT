module NFA = struct
  (* open Zdatatype
  open Zutils *)

  type t = {
    nodes : int list;
    edges : (int * char * int) list;
    initNode : int option;
    finalNodes : int list;
  }

  let is_initialized = ref false

  let _g : t ref =
    ref { nodes = []; edges = []; initNode = None; finalNodes = [] }

  let initGraph () =
    is_initialized := true;
    _g := { nodes = []; edges = []; initNode = None; finalNodes = [] }

  let fresh_nid () = List.length !_g.nodes

  let new_node () =
    let nid = fresh_nid () in
    _g := { !_g with nodes = nid :: !_g.nodes };
    nid

  let add_edge (x : int) (c : char) (y : int) =
    if not !is_initialized then ()
    else if List.mem x !_g.nodes && List.mem y !_g.nodes then
      _g := { !_g with edges = (x, c, y) :: !_g.edges }
    else ()

  let del_edge (x : int) (c : char) (y : int) =
    _g :=
      {
        !_g with
        edges =
          List.filter
            (fun (a, char, b) -> a != x || b != y || c != char)
            !_g.edges;
      }

  let set_init_node (x : int) = _g := { !_g with initNode = Some x }

  let set_final_node (x : int) =
    _g := { !_g with finalNodes = x :: !_g.finalNodes }

  let unset_final_node () = _g := { !_g with finalNodes = [] }

  open Zdatatype

  let next x =
    let edges =
      List.filter_map
        (fun (a, _, b) -> if a == x then Some b else None)
        !_g.edges
    in
    edges

  let prev x =
    let edges =
      List.filter_map
        (fun (a, _, b) -> if b == x then Some a else None)
        !_g.edges
    in
    edges

  let is_connected () =
    if not !is_initialized then false
    else
      let nodes = !_g.nodes in
      let reachable_all s =
        List.for_all (fun node -> IntSet.mem node s) nodes
      in
      match !_g.initNode with
      | None -> false
      | Some initNode ->
          if List.mem initNode nodes then
            let rec dijkstra reachable =
              if reachable_all reachable then true
              else
                let news =
                  IntSet.fold
                    (fun node news ->
                      IntSet.add_seq (List.to_seq (next node)) news)
                    reachable reachable
                in
                dijkstra news
            in
            dijkstra (IntSet.singleton initNode)
          else false

  let is_reach_final_node () =
    if not !is_initialized then false
    else
      let nodes = !_g.nodes in
      let reachable_all s =
        List.for_all (fun node -> IntSet.mem node s) nodes
      in
      match !_g.finalNodes with
      | [] -> false
      | finalNodes ->
          if List.for_all (fun node -> List.mem node nodes) finalNodes then
            let rec dijkstra reachable =
              if reachable_all reachable then true
              else
                let news =
                  IntSet.fold
                    (fun node news ->
                      IntSet.add_seq (List.to_seq (prev node)) news)
                    reachable reachable
                in
                dijkstra news
            in
            dijkstra (IntSet.of_list finalNodes)
          else false

  let is_nfa () =
    if
      (not !is_initialized)
      || (not (is_reach_final_node ()))
      || not (is_connected ())
    then false
    else
      let tab = Hashtbl.create 10 in
      List.fold_left
        (fun res (x, c, y) ->
          if res then true
          else
            match Hashtbl.find_opt tab (x, y) with
            | Some c' -> if Char.equal c c' then false else true
            | None ->
                let () = Hashtbl.add tab (x, y) c in
                false)
        false !_g.edges
end

open NFA
open Language
open Interpreter
(* open Zdatatype *)

let initHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  initGraph ()

let newNodeReqHandler (_ : msg) =
  let success = new_node () in
  send ("newNodeResp", [ mk_value_int success ])

let newNodeRespHandler (_ : msg) = ()

let addEdgeHandler (msg : msg) =
  let x, c, y =
    match msg.ev.args with
    | [ VConst (I x); VConst (C c); VConst (I y) ] -> (x, c, y)
    | _ -> _die [%here]
  in
  add_edge x c y

let delEdgeHandler (msg : msg) =
  let x, c, y =
    match msg.ev.args with
    | [ VConst (I x); VConst (C c); VConst (I y) ] -> (x, c, y)
    | _ -> _die [%here]
  in
  del_edge x c y

let setInitNodeHandler (msg : msg) =
  let x = match msg.ev.args with [ VConst (I x) ] -> x | _ -> _die [%here] in
  set_init_node x

let setFinalNodeHandler (msg : msg) =
  let x = match msg.ev.args with [ VConst (I x) ] -> x | _ -> _die [%here] in
  set_final_node x

let unsetFinalNodeHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  unset_final_node ()

let isNFAReqHandler (_ : msg) =
  let is_connected = is_connected () in
  send ("isNFAResp", [ mk_value_bool is_connected ])

let isNFARespHandler (_ : msg) = ()

let init () =
  register_handler "init" initHandler;
  register_handler "newNodeReq" newNodeReqHandler;
  register_handler "newNodeResp" newNodeRespHandler;
  register_handler "addEdge" addEdgeHandler;
  register_handler "delEdge" delEdgeHandler;
  register_handler "isNFAReq" isNFAReqHandler;
  register_handler "isNFAResp" isNFARespHandler;
  register_handler "setInitNode" setInitNodeHandler;
  register_handler "setFinalNode" setFinalNodeHandler;
  register_handler "unsetFinalNode" unsetFinalNodeHandler

let trace_is_not_nfa trace =
  let rec check = function
    | [] -> false
    | { ev = { op = "isNFAResp"; args = [ VConst (B is_nfa) ] }; _ } :: rest ->
        if is_nfa then true else check rest
    | _ :: rest -> check rest
  in
  not (check trace)

type nfa_bench_config = { numOp : int }

let randomTest { numOp } =
  let random_node () = send ("newNodeReq", []) in
  let random_new_edge () =
    let n1 = Sample.sample_by_ty (mk_p_abstract_ty "int") in
    let c = Sample.sample_by_ty (mk_p_abstract_ty "char") in
    let n2 = Sample.sample_by_ty (mk_p_abstract_ty "int") in
    send ("addEdge", [ n1; c; n2 ])
  in
  let random_del_edge () =
    let n1 = Sample.sample_by_ty (mk_p_abstract_ty "int") in
    let c = Sample.sample_by_ty (mk_p_abstract_ty "char") in
    let n2 = Sample.sample_by_ty (mk_p_abstract_ty "int") in
    send ("delEdge", [ n1; c; n2 ])
  in
  let random_set_init_node () =
    let n = Sample.sample_by_ty (mk_p_abstract_ty "int") in
    send ("setInitNode", [ n ])
  in
  let random_set_final_node () =
    let n = Sample.sample_by_ty (mk_p_abstract_ty "int") in
    send ("setFinalNode", [ n ])
  in
  let random_unset_final_node () = send ("unsetFinalNode", []) in
  let random_init () = send ("init", []) in
  let rec genOp restNum =
    if restNum <= 0 then ()
    else
      let () = Pp.printf "@{<yellow>restNum@}: %i\n" restNum in
      (match Random.int 7 with
      | 0 -> random_new_edge ()
      | 1 -> random_del_edge ()
      | 2 -> random_node ()
      | 3 -> random_set_init_node ()
      | 4 -> random_set_final_node ()
      | 5 -> random_unset_final_node ()
      | _ -> random_init ());
      genOp (restNum - 1)
  in
  let () = genOp numOp in
  let () = Pp.printf "@{<red>End with numOp@}\n%i\n" numOp in
  Effect.perform End
