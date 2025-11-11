module HConf = struct
  type sut = (int, int) Hashtbl.t

  type cmd =
    | Clear
    | Add of int * int
    | Remove of int
    | Find of int
    | Find_all of int
    | Replace of int * int
    | Mem of int
    | Length

  let gen_cmd () : cmd =
    let key = Random.int 20 in
    let value = Random.int 1000 in
    match Random.int 8 with
    | 0 -> Clear
    | 1 -> Add (key, value)
    | 2 -> Remove key
    | 3 -> Find key
    | 4 -> Find_all key
    | 5 -> Replace (key, value)
    | 6 -> Mem key
    | 7 -> Length
    | _ -> failwith "Impossible"

  let run c h : unit =
    try
      match c with
      | Clear -> Hashtbl.clear h
      | Add (k, v) -> Hashtbl.add h k v
      | Remove k -> Hashtbl.remove h k
      | Find k -> ignore (Hashtbl.find h k)
      | Find_all k -> ignore (Hashtbl.find_all h k)
      | Replace (k, v) -> Hashtbl.replace h k v
      | Mem k -> ignore (Hashtbl.mem h k)
      | Length -> ignore (Hashtbl.length h)
    with Not_found -> ()

  let init_sut () : sut = Hashtbl.create ~random:false 42
  let cleanup _ = ()
end

let domain_worker (sut : HConf.sut) (cmds_to_run : HConf.cmd list) : unit =
  (*
  Printf.printf "  [Domain %d] starting with %d commands...\n%!" 
    (Domain.self_id () :> int) (List.length cmds_to_run);
  *)
  List.iter (fun cmd -> HConf.run cmd sut) cmds_to_run;

  (*
  Printf.printf "  [Domain %d] finished.\n%!" (Domain.self_id () :> int);
  *)
  ()

let test_thread_safety_manually () =
  Printf.printf "Starting naive hashtable test...\n";

  let num_domains = 4 in
  let num_cmds_per_domain = 25_000 in

  let sut = HConf.init_sut () in

  let domain_workloads =
    List.init num_domains (fun _ ->
        List.init num_cmds_per_domain (fun _ -> HConf.gen_cmd ()))
  in

  Printf.printf "Spawning %d domains, each with %d commands...\n" num_domains
    num_cmds_per_domain;

  let domains =
    List.map
      (fun (workload : HConf.cmd list) ->
        Domain.spawn (fun () -> domain_worker sut workload))
      domain_workloads
  in

  List.iter Domain.join domains;

  HConf.cleanup sut;

  Printf.printf "\nRandom test did NOT fail\n";
  true

let test_fn () =
  Random.self_init ();
  if not (test_thread_safety_manually ()) then exit 1 else exit 0
