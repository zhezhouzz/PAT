open Language
open Zdatatype
open Common

let machine_gen gen_num =
  let machine_sets =
    List.slow_rm_dup String.equal @@ List.map (fun (_, s, _) -> s) gen_num
  in
  let machine_sets = List.map mk_p_machine_domain_var machine_sets in
  let indicator = "i"#:Nt.int_ty in
  let init =
    mk_p_assign_var indicator
      (mk_p_choose (mk_p_int (1 + List.length machine_sets)))
  in
  let aux i s =
    mk_p_if
      (mk_p_app le_func [ var_to_p_expr indicator; mk_p_int i ])
      [ PReturn (mk_p_choose (var_to_p_expr s)) ]
      []
  in
  let stmts = List.mapi aux machine_sets in
  let bot = PReturn mk_p_self in
  let block = [ init ] @ stmts @ [ bot ] in
  let closure = mk_p_closure [ indicator ] block in
  {
    name = "machine_gen";
    func_label = Plain;
    params = [];
    retty = p_machine_ty;
    closure;
  }

let gen_machine gen_num = mk_p_app (get_p_func_var @@ machine_gen gen_num) []

let int_gen =
  let bounds = [ (100, 10); (10, 100); (1, 1000); (1, 10000) ] in
  let size = List.fold_left (fun res (x, _) -> res + x) 0 bounds in
  let indicator = "i"#:Nt.int_ty in
  let init = mk_p_assign_var indicator @@ mk_p_choose (mk_p_int size) in
  let aux (sum, res) (bound, s) =
    let sum = sum + bound in
    let stmt =
      mk_p_if
        (mk_p_app le_func [ var_to_p_expr indicator; mk_p_int sum ])
        [ PReturn (mk_p_choose (mk_p_int s)) ]
        []
    in
    (sum, res @ [ stmt ])
  in
  let _, stmts = List.fold_left aux (0, []) bounds in
  let bot = PReturn (mk_p_int 0) in
  let block = [ init ] @ stmts @ [ bot ] in
  let closure = mk_p_closure [ indicator ] block in
  {
    name = "int_gen";
    func_label = Plain;
    params = [];
    retty = Nt.int_ty;
    closure;
  }

let gen_int = mk_p_app (get_p_func_var int_gen) []

let gen_bool =
  let choose = "choose"#:Nt.bool_ty in
  mk_p_app choose []

let generate_by_type gen_num nt =
  let open Nt in
  let rec aux nt =
    if equal_nt nt bool_ty then gen_bool
    else if equal_nt nt int_ty then gen_int
    else if equal_nt nt p_machine_ty then gen_machine gen_num
    else
      match nt with
      | Ty_tuple nts ->
          let es = List.map aux nts in
          mk_p_tuple es
      | Ty_record { fds; _ } ->
          let fds = List.map (fun x -> (x.x, aux x.ty)) fds in
          mk_p_record fds
      | _ -> _die [%here]
  in
  aux nt
