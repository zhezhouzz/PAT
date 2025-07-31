open Language
open Zdatatype

module SampleDomain = Map.Make (struct
  type t = Nt.t

  let compare = Nt.compare_nt
end)

let default_sample_domain =
  SampleDomain.of_seq @@ List.to_seq
  @@ [
       (Nt.int_ty, List.map (fun n -> I n) [ -1; 0; 1; 2; 3; 4 ]);
       (Nt.bool_ty, List.map (fun n -> B n) [ true; false ]);
       (* (mk_p_abstract_ty "rid", List.map (fun n -> I n) [ 1; 2; 3 ]);
       (mk_p_abstract_ty "aid", List.map (fun n -> I n) [ 4; 5; 6 ]);
       (mk_p_abstract_ty "tGid", List.map (fun n -> I n) [ 1; 2; 3 ]);
       (mk_p_abstract_ty "tKey", List.map (fun n -> I n) [ 4; 5; 6 ]);
       (mk_p_abstract_ty "tVal", List.map (fun n -> I n) [ 7; 8; 9 ]); *)
     ]

let choose_from_list l = List.nth l @@ Random.int (List.length l)

let sample qv =
  match qv.ty with
  (* | Nt.Ty_enum { enum_elems; enum_name } ->
          let elem = choose_from_list enum_elems in
          let c = Enum { enum_elems; enum_name; elem } in
          (qv.x, c) *)
  | _ -> (
      match SampleDomain.find_opt qv.ty default_sample_domain with
      | None ->
          let () =
            Printf.printf "cannot find sample domain of type (%s)\n"
              (Nt.layout qv.ty)
          in
          _die [%here]
      | Some cs -> (qv.x, choose_from_list cs))

let sample_phi store (vs, prop) =
  let rec aux (n : int) =
    if n <= 0 then
      let () =
        Printf.printf "vs: %s; prop: %s\n" (layout_qvs vs) (layout_prop prop)
      in
      _die_with [%here] "sample too many times"
    else
      let s = List.map sample vs in
      let store' = StrMap.add_seq (List.to_seq s) store in
      if Store.eval_prop store' prop then s else aux (n - 1)
  in
  aux 10000

(* let mk_assume store (vs, prop) =
  let s = sample_phi store (vs, prop) in
  StrMap.add_seq (List.to_seq s) store *)
