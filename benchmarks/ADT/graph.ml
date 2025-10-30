module ConnectedGraph = struct
  (* open Zdatatype
  open Zutils *)

  type t = { nodes : int list; edges : (int * int) list }

  let is_initialized = ref false
  let _g : t ref = ref { nodes = []; edges = [] }

  let initGraph () =
    is_initialized := true;
    _g := { nodes = []; edges = [] }

  let fresh_nid () =
    (* let () =
      Pp.printf "@{<yellow>nodes(%i):@} %s\n" (List.length !_g.nodes)
        (List.split_by ", " (fun x -> string_of_int x) !_g.nodes)
    in *)
    List.length !_g.nodes

  let new_node () =
    let nid = fresh_nid () in
    _g := { !_g with nodes = nid :: !_g.nodes };
    nid

  let add_edge (x : int) (y : int) =
    if not !is_initialized then ()
    else if List.mem x !_g.nodes && List.mem y !_g.nodes then
      _g := { !_g with edges = (x, y) :: !_g.edges }
    else ()

  let del_edge (x : int) (y : int) =
    _g :=
      {
        !_g with
        edges = List.filter (fun (a, b) -> a != x || b != y) !_g.edges;
      }

  open Zdatatype
  (* open Zutils *)

  let next x =
    let edges =
      List.filter_map (fun (a, b) -> if a == x then Some b else None) !_g.edges
    in
    edges

  let is_connected () =
    if not !is_initialized then false
    else
      let nodes = !_g.nodes in
      let reachable_all s =
        List.for_all (fun node -> IntSet.mem node s) nodes
      in
      (* let () =
        Printf.printf "nodes: %s\n"
          (List.split_by ", " (fun x -> string_of_int x) nodes)
      in *)
      if List.mem 0 nodes then
        let rec dijkstra reachable =
          if reachable_all reachable then true
          else
            let news =
              IntSet.fold
                (fun node news -> IntSet.add_seq (List.to_seq (next node)) news)
                reachable reachable
            in
            (* let () =
              Printf.printf "news: %s\n"
                (List.split_by ", "
                   (fun x -> string_of_int x)
                   (IntSet.elements news))
            in *)
            if IntSet.cardinal news == IntSet.cardinal reachable then false
            else dijkstra news
        in
        dijkstra (IntSet.singleton 0)
      else false
end

open ConnectedGraph
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
  let x, y =
    match msg.ev.args with
    | [ VConst (I x); VConst (I y) ] -> (x, y)
    | _ -> _die [%here]
  in
  add_edge x y

let delEdgeHandler (msg : msg) =
  let x, y =
    match msg.ev.args with
    | [ VConst (I x); VConst (I y) ] -> (x, y)
    | _ -> _die [%here]
  in
  del_edge x y

let isConnectedReqHandler (_ : msg) =
  let is_connected = is_connected () in
  send ("isConnectedResp", [ mk_value_bool is_connected ])

let isConnectedRespHandler (_ : msg) = ()

let init () =
  register_handler "init" initHandler;
  register_handler "newNodeReq" newNodeReqHandler;
  register_handler "newNodeResp" newNodeRespHandler;
  register_handler "addEdge" addEdgeHandler;
  register_handler "delEdge" delEdgeHandler;
  register_handler "isConnectedReq" isConnectedReqHandler;
  register_handler "isConnectedResp" isConnectedRespHandler

open Zdatatype

let trace_is_not_connected trace =
  let rec check nodeSet = function
    | [] -> false
    | { ev = { op = "newNodeResp"; args = [ VConst (I node) ] }; _ } :: rest ->
        let nodeSet = IntSet.add node nodeSet in
        check nodeSet rest
    | { ev = { op = "isConnectedResp"; args = [ VConst (B is_connected) ] }; _ }
      :: rest ->
        if IntSet.cardinal nodeSet < 2 then false
        else if is_connected then true
        else check nodeSet rest
    | _ :: rest -> check nodeSet rest
  in
  not (check IntSet.empty trace)

type connected_graph_bench_config = { numOp : int }

let parse_config config =
  let numOp = get_config_value config "numOp" in
  { numOp }

let randomTest config =
  let { numOp } = parse_config config in
  let random_node () = send ("newNodeReq", []) in
  let random_new_edge () =
    let n1 = Sample.sample_by_ty (mk_p_abstract_ty "int") in
    let n2 = Sample.sample_by_ty (mk_p_abstract_ty "int") in
    send ("addEdge", [ n1; n2 ])
  in
  let random_del_edge () =
    let n1 = Sample.sample_by_ty (mk_p_abstract_ty "int") in
    let n2 = Sample.sample_by_ty (mk_p_abstract_ty "int") in
    send ("delEdge", [ n1; n2 ])
  in
  let random_init () = send ("init", []) in
  let rec genOp restNum =
    if restNum <= 0 then ()
    else
      let () = Pp.printf "@{<yellow>restNum@}: %i\n" restNum in
      (match Random.int 4 with
      | 0 -> random_new_edge ()
      | 1 -> random_del_edge ()
      | 2 -> random_node ()
      | _ -> random_init ());
      genOp (restNum - 1)
  in
  let () = genOp numOp in
  let () = Pp.printf "@{<red>End with numOp@}\n%i\n" numOp in
  Effect.perform End

let test_env =
  {
    init_test_env = init;
    default_test_prog = [];
    property = trace_is_not_connected;
    random_test_gen = randomTest;
  }
