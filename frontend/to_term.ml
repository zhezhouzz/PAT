open Ast
open Zdatatype

let layout_typed_value v = Syntax.layout_value v.x

let layout_term term =
  let rec aux = function
    | CVal v -> layout_typed_value v
    | CLetE { lhs = []; rhs; body } -> spf "%s;\n%s" (aux rhs.x) (aux body.x)
    | CLetE { lhs; rhs; body } ->
        spf "let (%s) = %s in\n%s"
          (List.split_by_comma _get_x lhs)
          (aux rhs.x) (aux body.x)
    | CAppOp { op; args } ->
        spf "%s %s" op.x (List.split_by " " layout_typed_value args)
    | CObs { op; prop } ->
        if is_true prop then spf "obs %s" op.x
        else spf "obs %s; assert %s" op.x (layout_prop prop)
    | CGen { op; args } ->
        spf "gen %s(%s)" op.x (List.split_by " " layout_typed_value args)
    (* | CGen { op; args } ->
        let fds = Nt.get_record_types op.ty in
        let l = _safe_combine [%here] fds args in
        spf "gen %s(%s)" op.x
          (List.split_by " "
             (fun (x, v) -> spf "%s = %s" x.x (layout_typed_value v))
             l) *)
    | CUnion es -> List.split_by " ⊕\n" aux (List.map _get_x es)
    | CAssume (nt, phi) ->
        spf "assume[%s] %s" (Nt.layout (Ty_tuple nt)) (layout_prop phi)
    | CAssertP phi -> spf "assert %s" (layout_prop phi)
    | CWhile { body; cond } ->
        spf "do{\n%s\n} while(%s)\n" (aux body.x) (layout_prop cond)
    | KStar { body } -> spf "while(*) {\n%s\n}" (aux body.x)
  in
  aux term
