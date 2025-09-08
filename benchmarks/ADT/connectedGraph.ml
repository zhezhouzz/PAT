module ConnectedGraph = struct
  type t = { nodes : int list; edges : (int * int) list }

  let _g : t ref = ref { nodes = []; edges = [] }
  let init () = _g := { nodes = []; edges = [] }
  let fresh_nid = List.length !_g.nodes

  let new_node () =
    let nid = fresh_nid in
    _g := { !_g with nodes = fresh_nid :: !_g.nodes };
    nid

  let add_edge (x : int) (y : int) =
    if List.mem x !_g.nodes && List.mem y !_g.nodes then
      _g := { !_g with edges = (x, y) :: !_g.edges }
    else ()

  let del_edge (x : int) (y : int) =
    _g :=
      {
        !_g with
        edges = List.filter (fun (a, b) -> a != x || b != y) !_g.edges;
      }

  open Zdatatype

  let next x =
    let edges =
      List.filter_map (fun (a, b) -> if a == x then Some b else None) !_g.edges
    in
    edges

  let is_connected () =
    let nodes = !_g.nodes in
    let reachable_all s = List.for_all (fun node -> IntSet.mem node s) nodes in
    if List.mem 0 nodes then
      let rec dijkstra reachable =
        if reachable_all reachable then true
        else
          let news =
            IntSet.fold
              (fun node news -> IntSet.add_seq (List.to_seq (next node)) news)
              reachable reachable
          in
          dijkstra news
      in
      dijkstra (IntSet.singleton 0)
    else false
end
