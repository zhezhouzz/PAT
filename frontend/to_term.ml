open Ast
open Zdatatype

let layout_typed_value v = Syntax.layout_value v.x

let layout_term term =
  let indent_spf n str = spf "%s%s" (String.init (n * 2) (fun _ -> ' ')) str in
  let rec aux indent = function
    | CVal v -> indent_spf indent (layout_typed_value v)
    | CLetE { lhs = []; rhs; body } ->
        indent_spf indent (spf "%s;\n%s" (aux 0 rhs.x) (aux indent body.x))
    | CLetE { lhs; rhs; body } ->
        indent_spf indent
          (spf "let (%s) = %s in\n%s"
             (List.split_by_comma _get_x lhs)
             (aux 0 rhs.x) (aux indent body.x))
    | CAppOp { op; args = [ arg1; arg2 ] }
      when List.exists (String.equal op.x)
             [ "=="; ">="; ">"; "<="; "<"; "!="; "+"; "-"; "*"; "/" ] ->
        indent_spf indent
          (spf "%s%s%s" (layout_typed_value arg1) op.x (layout_typed_value arg2))
    | CAppOp { op; args } ->
        indent_spf indent
          (spf "%s %s" op.x (List.split_by " " layout_typed_value args))
    | CObs { op; prop } ->
        if is_true prop then indent_spf indent (spf "obs %s" op.x)
        else indent_spf indent (spf "obs %s; assert %s" op.x (layout_prop prop))
    | CGen { op; args } ->
        indent_spf indent
          (spf "%s %s" op.x (List.split_by " " layout_typed_value args))
    (* | CGen { op; args } ->
        let fds = Nt.get_record_types op.ty in
        let l = _safe_combine [%here] fds args in
        spf "gen %s(%s)" op.x
          (List.split_by " "
             (fun (x, v) -> spf "%s = %s" x.x (layout_typed_value v))
             l) *)
    | CUnion es -> List.split_by "\n⊕\n" (aux indent) (List.map _get_x es)
    | CAssume (nt, phi) ->
        indent_spf indent
          (spf "assume[%s] %s" (Nt.layout (Ty_tuple nt)) (layout_prop phi))
    | CAssertP phi -> indent_spf indent (spf "assert %s" (layout_prop phi))
    (* | CWhile { body; cond } ->
        spf "do{\n%s\n} while(%s)\n" (aux body.x) (layout_prop cond)
    | KStar { body } -> spf "while(*) {\n%s\n}" (aux body.x) *)
    | CFix { retBranch; recBranch } ->
        spf "fix[%s] (%s: int) (%s: int) =\n%s\n%s\n%s\n%s" default_self_name
          default_iter_var.x default_bound_var.x
          (indent_spf (indent + 1)
             (spf "if %s >= %s then" default_iter_var.x default_bound_var.x))
          (aux (indent + 2) retBranch.x)
          (indent_spf (indent + 1) "else")
          (aux (indent + 2) recBranch.x)
    | CFixApp { iterV; boundV; cfix } ->
        let is_defined = match cfix with Some _ -> "def" | None -> "undef" in
        indent_spf indent
          (spf "%s[%s] %s %s" default_self_name is_defined (aux 0 iterV.x)
             (layout_typed_value boundV))
  in
  aux 0 term
