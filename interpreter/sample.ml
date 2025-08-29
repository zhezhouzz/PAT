open Language
open Zdatatype

module SampleDomain = Map.Make (struct
  type t = Nt.t

  let compare = Nt.compare_nt
end)

let default_sample_domain =
  SampleDomain.of_seq @@ List.to_seq
  @@ [
       (Nt.int_ty, List.map (fun n -> VConst (I n)) [ -1; 0; 1; 2; 3; 4 ]);
       (Nt.bool_ty, List.map (fun n -> VConst (B n)) [ true; false ]);
       ( Nt.string_ty,
         List.map (fun n -> VConst (S n)) [ "a"; "b"; "c"; "d"; "e" ] );
       ( mk_p_abstract_ty "Path.t",
         List.map
           (fun n -> VConst (S n))
           [
             "/a";
             "/a/b";
             "/a/b/c";
             "/a/b/c/d";
             "/a/b/c/d/e";
             "/b";
             "/b/a";
             "/b/a/b";
             "/b/a/b/c";
             "/b/a/b/c/d";
             "/b/a/b/c/d/e";
             "/c";
             "/c/a";
             "/c/a/b";
             "/c/a/b/d";
             "/c/a/b/d/e";
             "/c/a/b/d/e/f";
           ] );
       ( mk_p_abstract_ty "stlcTy",
         List.map
           (fun n -> VCStlcTy n)
           [
             StlcInt;
             StlcArrow (StlcInt, StlcInt);
             StlcArrow (StlcInt, StlcArrow (StlcInt, StlcInt));
             StlcArrow (StlcArrow (StlcInt, StlcInt), StlcInt);
           ] );
       ( mk_p_abstract_ty "filterOption",
         List.map
           (fun n -> VConst (S n))
           [ "ActiveOp"; "CompletedOp"; "AllOp"; "NoneOp" ] );
       (* (mk_p_abstract_ty "rid", List.map (fun n -> I n) [ 1; 2; 3 ]);
       (mk_p_abstract_ty "aid", List.map (fun n -> I n) [ 4; 5; 6 ]);
       (mk_p_abstract_ty "tGid", List.map (fun n -> I n) [ 1; 2; 3 ]);
       (mk_p_abstract_ty "tKey", List.map (fun n -> I n) [ 4; 5; 6 ]);
       (mk_p_abstract_ty "tVal", List.map (fun n -> I n) [ 7; 8; 9 ]); *)
     ]

let choose_from_list l = List.nth l @@ Random.int (List.length l)

let sample_by_ty ty =
  match SampleDomain.find_opt ty default_sample_domain with
  | None ->
      let () =
        Printf.printf "cannot find sample domain of type (%s)\n" (Nt.layout ty)
      in
      _die [%here]
  | Some cs -> choose_from_list cs

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
