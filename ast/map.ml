open ParseTree
open AutomataLibrary

let map_cty f = function { nty; phi } -> { nty = f nty; phi = map_prop f phi }

let rec map_rich_srl_pat f = function
  | RtyBase cty -> RtyBase (map_cty f cty)
  | RtyHAF { history; adding; future } ->
      RtyHAF
        {
          history = map_rich_regex (map_sevent f) history;
          adding = map_rich_regex (map_sevent f) adding;
          future = map_rich_regex (map_sevent f) future;
        }
  | RtyHAParallel { history; adding_se; parallel } ->
      RtyHAParallel
        {
          history = map_rich_regex (map_sevent f) history;
          adding_se = map_sevent f adding_se;
          parallel = List.map (map_sevent f) parallel;
        }
  | RtyGArr { arg; argnty; retrty } ->
      RtyGArr { arg; argnty = f argnty; retrty = map_rich_srl_pat f retrty }
  | RtyArr { arg; argcty; retrty } ->
      RtyArr
        { arg; argcty = map_cty f argcty; retrty = map_rich_srl_pat f retrty }
  | RtyInter (p1, p2) -> RtyInter (map_rich_srl_pat f p1, map_rich_srl_pat f p2)

let rec map_value f = function
  | VVar v -> VVar v#=>f
  | VConst c -> VConst c
  | VCStlcTy ty -> VCStlcTy ty
  | VCIntList xs -> VCIntList xs

and typed_map_value f { x; ty } = { x = map_value f x; ty = f ty }

let rec map_term f = function
  | CVal v -> CVal (typed_map_value f v)
  | CLetE { lhs; rhs; body } ->
      CLetE
        {
          lhs = List.map (fun x -> x#=>f) lhs;
          rhs = typed_map_term f rhs;
          body = typed_map_term f body;
        }
  | CAppOp { op; args } ->
      CAppOp { op; args = List.map (typed_map_value f) args }
  | CObs { op; prop } -> CObs { op; prop = map_prop f prop }
  | CGen { op; args } -> CGen { op; args = List.map (typed_map_value f) args }
  | CUnion es -> CUnion (List.map (typed_map_term f) es)
  | CAssertP phi -> CAssertP (map_prop f phi)
  | CAssume (args, prop) -> CAssume (List.map f args, map_prop f prop)
  | CWhile { body; cond } ->
      CWhile { body = typed_map_term f body; cond = map_prop f cond }

and typed_map_term f { x; ty } = { x = map_term f x; ty = f ty }

let map_trace_elem f { op; args } = { op; args = List.map f args }
let map_trace f = List.map (map_trace_elem f)

let map_syn_goal f { qvs; prop } =
  {
    qvs = List.map (fun x -> x#=>f) qvs;
    prop = map_rich_regex (map_sevent f) prop;
  }

let map_item f = function
  | PrimDecl { name; nt } -> PrimDecl { name; nt = f nt }
  | MsgNtDecl { msgkind; name; nt } -> MsgNtDecl { msgkind; name; nt = f nt }
  | MsgDecl { name; pat } -> MsgDecl { name; pat = map_rich_srl_pat f pat }
  | SynGoal syn_goal -> SynGoal (map_syn_goal f syn_goal)
  | PrAxiom { name; prop } -> PrAxiom { name; prop = map_prop f prop }
