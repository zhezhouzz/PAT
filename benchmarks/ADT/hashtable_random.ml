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
  [@@deriving show { with_path = false }]

  let pp_cmd = pp_cmd
  let show_cmd = show_cmd
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

let gen_cmd (s : HConf.state) : HConf.cmd Gen.t =
  let key_gen =
    if s = [] then Gen.int_range 0 10
    else Gen.(oneof [ oneofl (List.map fst s); int_range 0 10 ])
  in
  let val_gen = Gen.int_range 0 100 in
  Gen.oneof
    [
      Gen.return HConf.Clear;
      Gen.map2 (fun k v -> HConf.Add (k, v)) key_gen val_gen;
      Gen.map (fun k -> HConf.Remove k) key_gen;
      Gen.map (fun k -> HConf.Find k) key_gen;
      Gen.map (fun k -> HConf.Find_all k) key_gen;
      Gen.map2 (fun k v -> HConf.Replace (k, v)) key_gen val_gen;
      Gen.map (fun k -> HConf.Mem k) key_gen;
      Gen.return HConf.Length;
    ]

let gen_cmd_list (rand_state : Random.State.t) (max_len : int) : HConf.cmd list
    =
  let len = (Gen.int_range 1 max_len) rand_state in

  let rec loop n current_model_state acc_cmds =
    if n = 0 then List.rev acc_cmds
    else
      let cmd = (gen_cmd current_model_state) rand_state in
      let next_model_state = HConf.next_state cmd current_model_state in
      loop (n - 1) next_model_state (cmd :: acc_cmds)
  in
  loop len HConf.init_state []

let run_test (cmds : HConf.cmd list) : (bool, string) result =
  let sut = HConf.init_sut () in

  let rec loop current_model_state remaining_cmds =
    match remaining_cmds with
    | [] -> Ok true
    | cmd :: tl ->
        if not (HConf.precond cmd current_model_state) then
          Error
            (Printf.sprintf "Precondition failed for: %s" (HConf.show_cmd cmd))
        else
          let res = HConf.run cmd sut in

          if HConf.postcond cmd current_model_state res then
            let next_model_state = HConf.next_state cmd current_model_state in
            loop next_model_state tl
          else
            Error
              (Printf.sprintf "Postcondition failed for command: %s"
                 (HConf.show_cmd cmd))
  in

  let result = loop HConf.init_state cmds in
  HConf.cleanup sut;
  result

let test_fn () =
  let count = 1000 in
  let max_cmd_list_len = 100 in
  let rand_state = Random.State.make_self_init () in

  Printf.printf "Running %d tests (max_len=%d) without shrinking...\n" count
    max_cmd_list_len;
  let dots = count / 20 in

  let rec run_n_times n_remaining =
    if n_remaining = 0 then (
      Printf.printf "\nSuccess: Ran %d tests.\n" count;
      true)
    else
      let cmds = gen_cmd_list rand_state max_cmd_list_len in

      match run_test cmds with
      | Ok true ->
          if (count - n_remaining + 1) mod dots = 0 then Printf.printf ".%!";
          run_n_times (n_remaining - 1)
      | Ok false ->
          Printf.printf
            "\nError: Test runner returned 'Ok false'. This is unexpected.\n";
          false
      | Error msg ->
          Printf.printf "\n--- FAILED! ---\n";
          Printf.printf "Failure on test %d.\n" (count - n_remaining + 1);
          Printf.printf "Error: %s\n" msg;
          Printf.printf "\nFailing command list (%d commands):\n"
            (List.length cmds);
          List.iter
            (fun cmd -> Printf.printf "  %s\n" (HConf.show_cmd cmd))
            cmds;
          false
  in

  run_n_times count

let () = if not (test_fn ()) then exit 1 else exit 0
