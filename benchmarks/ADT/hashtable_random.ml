open QCheck
open STM

(*
Taken from the muticoretests repo and modified to match the spec seen in hashtable.ml.
 *)

module HConf = struct
  type sut = (int, int) Hashtbl.t
  type state = (int * int) list

  type cmd =
    | Clear
    | Add of int * int
    | Remove of int
    | Find of int
    | Find_all of int
    | Replace of int * int
    | Mem of int
    | Length

  let pp_cmd par fmt x =
    let open Util.Pp in
    match x with
    | Clear -> cst0 "Clear" fmt
    | Add (x, y) -> cst2 pp_int pp_int "Add" par fmt x y
    | Remove x -> cst1 pp_int "Remove" par fmt x
    | Find x -> cst1 pp_int "Find" par fmt x
    | Find_all x -> cst1 pp_int "Find_all" par fmt x
    | Replace (x, y) -> cst2 pp_int pp_int "Replace" par fmt x y
    | Mem x -> cst1 pp_int "Mem" par fmt x
    | Length -> cst0 "Length" fmt

  let show_cmd = Util.Pp.to_show pp_cmd
  let init_sut () = Hashtbl.create ~random:false 42
  let cleanup _ = ()

  let arb_cmd s =
    let key_gen =
      if s = [] then Gen.int_range 0 10
      else Gen.(oneof [ oneofl (List.map fst s); int_range 0 10 ])
    in
    let val_gen = Gen.int_range 0 100 in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [
           Gen.return Clear;
           Gen.map2 (fun k v -> Add (k, v)) key_gen val_gen;
           Gen.map (fun k -> Remove k) key_gen;
           Gen.map (fun k -> Find k) key_gen;
           Gen.map (fun k -> Find_all k) key_gen;
           Gen.map2 (fun k v -> Replace (k, v)) key_gen val_gen;
           Gen.map (fun k -> Mem k) key_gen;
           Gen.return Length;
         ])

  let next_state c s =
    match c with
    | Clear -> []
    | Add (k, v) -> (k, v) :: s
    | Remove k -> List.remove_assoc k s
    | Find _ | Find_all _ -> s
    | Replace (k, v) -> (k, v) :: List.remove_assoc k s
    | Mem _ | Length -> s

  let run c h =
    match c with
    | Clear -> Res (unit, Hashtbl.clear h)
    | Add (k, v) -> Res (unit, Hashtbl.add h k v)
    | Remove k -> Res (unit, Hashtbl.remove h k)
    | Find k -> Res (result int exn, protect (Hashtbl.find h) k)
    | Find_all k -> Res (list int, Hashtbl.find_all h k)
    | Replace (k, v) -> Res (unit, Hashtbl.replace h k v)
    | Mem k -> Res (bool, Hashtbl.mem h k)
    | Length -> Res (int, Hashtbl.length h)

  let init_state = []
  let precond _ _ = true

  let postcond c (s : state) res =
    match (c, res) with
    | Clear, Res ((Unit, _), _)
    | Add (_, _), Res ((Unit, _), _)
    | Replace (_, _), Res ((Unit, _), _) ->
        true
    | Remove _, Res ((Unit, _), _) -> true
    | Find k, Res ((Result (Int, Exn), _), r) -> (
        r = try Ok (List.assoc k s) with Not_found -> Error Not_found)
    | Find_all k, Res ((List Int, _), r) ->
        let rec find_all h =
          match h with
          | [] -> []
          | (k', v') :: h' -> if k = k' then v' :: find_all h' else find_all h'
        in
        r = find_all s
    | Mem k, Res ((Bool, _), r) -> r = List.mem_assoc k s
    | Length, Res ((Int, _), r) -> r = List.length s
    | _ -> false
end

module HTest_dom = STM_domain.Make (HConf)

let test_fn () =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [ HTest_dom.agree_test_par ~count ~name:"STM Hashtbl test parallel" ])
