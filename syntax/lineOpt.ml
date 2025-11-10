open Zdatatype
(* open AutomataLibrary *)

(* open Common *)
open Ast

let local_vars_in_line line =
  let rec aux acc = function
    | [] -> acc
    | LineAct act :: rest -> aux (act.aargs @ acc) rest
    | LineStarMultiChar _ :: rest -> aux acc rest
  in
  aux [] line.elems

let fvs_in_line line =
  let lvars = local_vars_in_line line in
  let fvs = fv_prop line.gprop in
  List.filter
    (fun x -> not (List.exists (fun y -> String.equal x.x y.x) lvars))
    fvs

let get_assigns conjs vs =
  let assignments =
    List.filter_map
      (fun x ->
        let l = List.filter_map (fun prop -> is_eq_phi x prop) conjs in
        match l with [] -> None | lit :: _ -> Some (x.x, lit))
      vs
  in
  if List.length assignments == 0 then None else Some assignments

let rec optimize_prop (lvars, prop) =
  let fvs = fv_prop prop in
  let fvs =
    List.filter
      (fun x -> not (List.exists (fun y -> String.equal x.x y.x) lvars))
      fvs
  in
  match fvs with
  | [] -> prop
  | _ -> (
      match get_assigns (prop_to_conjuncts prop) fvs with
      | None -> prop
      | Some ass ->
          (* let () = Printf.printf "fvs: %s\n" (layout_qvs fvs) in *)
          (* let () = Printf.printf "ass: %s\n" (List.split_by_comma layout_qv ass) in *)
          let gprop = msubst subst_prop_instance ass prop in
          let gprop = simpl_eq_in_prop gprop in
          (* let () = Printf.printf "gprop: %s\n" (layout_prop gprop) in *)
          optimize_prop (lvars, gprop))

let optimize_line line =
  let lvars = local_vars_in_line line in
  let gprop = optimize_prop (lvars, line.gprop) in
  { line with gprop }
