open Zdatatype
open Zutils

module BasicIFC = struct
  type lvl = Public | Private [@@deriving eq, ord]
  type cell = { lvl : lvl; vi : int } [@@deriving ord]

  let equal_cell c1 c2 =
    match (c1, c2) with
    | { lvl = Public; vi = vi1 }, { lvl = Public; vi = vi2 } -> vi1 == vi2
    | { lvl = Private; _ }, { lvl = Private; _ } -> true
    | _ -> false

  type value = PublicV of int | PrivateV of int * int [@@deriving ord]

  type instr = Push of value | Pop | Load | Store | Add | Noop | Halt
  [@@deriving ord]

  type ifcCtx = { stack : cell list; memory : cell IntMap.t }
  [@@deriving eq, ord]

  let layout_lvl = function Public -> "Pub" | Private -> "Pri"

  let layout_value = function
    | PublicV v -> spf "Pub(%i)" v
    | PrivateV (v1, v2) -> spf "Pri(%i, %i)" v1 v2

  let layout_cell = function { lvl; vi } -> spf "%s(%i)" (layout_lvl lvl) vi

  let layout_instr = function
    | Push v -> spf "Push(%s)" (layout_value v)
    | Pop -> "Pop"
    | Load -> "Load"
    | Store -> "Store"
    | Add -> "Add"
    | Noop -> "Noop"
    | Halt -> "Halt"

  let layout_ifcCtx { stack; memory } =
    spf "stack: %s\nmemory:\n%s"
      (List.split_by_comma layout_cell stack)
      (IntMap.to_kv_list memory
      |> List.sort (fun (k1, _) (k2, _) -> compare k1 k2)
      |> List.map (fun (k, v) -> spf "%i: %s" k (layout_cell v))
      |> String.concat "\n")

  let layout_instrs instrs = List.split_by_comma layout_instr instrs

  let lvl_or l1 l2 =
    match (l1, l2) with
    | Public, _ -> l2
    | _, Public -> l1
    | Private, Private -> Private

  let lvl_leq l1 l2 =
    match (l1, l2) with Private, Public -> false | _, _ -> true

  let default_cell = { lvl = Public; vi = 0 }

  let init () =
    let memory = IntMap.of_list (List.init 10 (fun i -> (i, default_cell))) in
    { stack = []; memory }

  let rule_noop (ctx : ifcCtx) = Some ctx

  let rule_push (c : cell) (ctx : ifcCtx) =
    Some { ctx with stack = c :: ctx.stack }

  let rule_pop (ctx : ifcCtx) =
    match ctx.stack with [] -> None | _ :: stack -> Some { ctx with stack }

  let rule_load_star ({ stack; memory } : ifcCtx) =
    match stack with
    | [] -> None
    | c1 :: stack -> (
        match IntMap.find_opt memory c1.vi with
        | None -> None
        | Some c2 -> Some { stack = c2 :: stack; memory })

  let rule_load ({ stack; memory } : ifcCtx) =
    match stack with
    | [] -> None
    | c1 :: stack -> (
        match IntMap.find_opt memory c1.vi with
        | None -> None
        | Some c2 ->
            let c = { lvl = c1.lvl; vi = c2.vi } in
            Some { stack = c :: stack; memory })

  let rule_store_star_ab ({ stack; memory } : ifcCtx) =
    match stack with
    | [] | [ _ ] -> None
    | c1 :: c2 :: stack ->
        let memory = IntMap.update c1.vi (fun _ -> Some c2) memory in
        let () =
          Pp.printf "@{<green>store:@} %s\n" (layout_ifcCtx { stack; memory })
        in
        Some { stack; memory }

  let rule_store_star_b ({ stack; memory } : ifcCtx) =
    match stack with
    | [] | [ _ ] -> None
    | c1 :: c2 :: stack ->
        let c = { lvl = lvl_or c1.lvl c2.lvl; vi = c2.vi } in
        let memory = IntMap.update c1.vi (fun _ -> Some c) memory in
        Some { stack; memory }

  let rule_store ({ stack; memory } : ifcCtx) =
    match stack with
    | [] | [ _ ] -> None
    | c1 :: c2 :: stack -> (
        match IntMap.find_opt memory c1.vi with
        | None -> None
        | Some orginal_c ->
            if lvl_leq c1.lvl orginal_c.lvl then
              let c = { lvl = lvl_or c1.lvl c2.lvl; vi = c2.vi } in
              let memory = IntMap.update c1.vi (fun _ -> Some c) memory in
              Some { stack; memory }
            else None)

  let rule_add_star ({ stack; memory } : ifcCtx) =
    match stack with
    | [] | [ _ ] -> None
    | c1 :: c2 :: stack ->
        Some { stack = { lvl = Public; vi = c1.vi + c2.vi } :: stack; memory }

  let rule_add ({ stack; memory } : ifcCtx) =
    match stack with
    | [] | [ _ ] -> None
    | c1 :: c2 :: stack ->
        Some
          {
            stack = { lvl = lvl_or c1.lvl c2.lvl; vi = c1.vi + c2.vi } :: stack;
            memory;
          }

  type rule_set = {
    loadRule : ifcCtx -> ifcCtx option;
    storeRule : ifcCtx -> ifcCtx option;
    addRule : ifcCtx -> ifcCtx option;
  }

  let opt_2 e1 e2 =
    match (e1, e2) with Some e1, Some e2 -> Some (e1, e2) | _, _ -> None

  let _halt = ref false

  let step ruleset ((ctx1, ctx2) : ifcCtx * ifcCtx) (instr : instr) =
    if !_halt then Some (ctx1, ctx2)
    else
      let () = Pp.printf "@{<green>ctx1:@} %s\n" (layout_ifcCtx ctx1) in
      let () = Pp.printf "@{<green>ctx2:@} %s\n" (layout_ifcCtx ctx2) in
      match instr with
      | Push (PublicV v) ->
          let c = { lvl = Public; vi = v } in
          opt_2 (rule_push c ctx1) (rule_push c ctx2)
      | Push (PrivateV (v1, v2)) ->
          let c1 = { lvl = Private; vi = v1 } in
          let c2 = { lvl = Private; vi = v2 } in
          opt_2 (rule_push c1 ctx1) (rule_push c2 ctx2)
      | Pop -> opt_2 (rule_pop ctx1) (rule_pop ctx2)
      | Load -> opt_2 (ruleset.loadRule ctx1) (ruleset.loadRule ctx2)
      | Store -> opt_2 (ruleset.storeRule ctx1) (ruleset.storeRule ctx2)
      | Add -> opt_2 (ruleset.addRule ctx1) (ruleset.addRule ctx2)
      | Noop -> Some (ctx1, ctx2)
      | Halt ->
          _halt := true;
          Some (ctx1, ctx2)

  let rec multi_step ruleset (ctx1, ctx2) instrs =
    let () = Printf.printf "ctx1: %s\n" (layout_ifcCtx ctx1) in
    let () = Printf.printf "ctx2: %s\n" (layout_ifcCtx ctx2) in
    let () = Printf.printf "instrs: %s\n" (layout_instrs instrs) in
    match instrs with
    | [] -> _die_with [%here] "never"
    | [ Halt ] -> Some (ctx1, ctx2)
    | instr :: instrs -> (
        match step ruleset (ctx1, ctx2) instr with
        | Some (ctx1, ctx2) -> multi_step ruleset (ctx1, ctx2) instrs
        | None -> None)

  let tmp_instrs = ref []
  let append_instr instr = tmp_instrs := !tmp_instrs @ [ instr ]
  let clear_instrs () = tmp_instrs := []

  let calculus_stack_depth () =
    let rec aux (n, instrs) =
      match instrs with
      | [] -> n
      | instr :: instrs -> (
          match instr with
          | Push _ -> aux (n + 1, instrs)
          | Pop -> aux (n - 1, instrs)
          | Load -> aux (n, instrs)
          | Store -> aux (n - 2, instrs)
          | Add -> aux (n - 1, instrs)
          | Noop -> aux (n, instrs)
          | Halt -> aux (n, instrs))
    in
    aux (0, !tmp_instrs)

  let ruleset_wrong_load =
    { loadRule = rule_load_star; storeRule = rule_store; addRule = rule_add }

  let ruleset_wrong_store_ab =
    { loadRule = rule_load; storeRule = rule_store_star_ab; addRule = rule_add }

  let ruleset_wrong_store_b =
    { loadRule = rule_load; storeRule = rule_store_star_b; addRule = rule_add }

  let ruleset_wrong_add =
    { loadRule = rule_load; storeRule = rule_store; addRule = rule_add_star }

  let check_eeni ruleset_variant =
    let ctx1 = init () in
    let ctx2 = init () in
    match multi_step ruleset_variant (ctx1, ctx2) !tmp_instrs with
    | Some (ctx1, ctx2) ->
        let () = Pp.printf "@{<green>success@}\n" in
        equal_ifcCtx ctx1 ctx2
    | None ->
        let () = Pp.printf "@{<red>runtime error@}\n" in
        true

  type runtime = { ctx : ifcCtx * ifcCtx; runtimeError : bool }

  let _ctx = ref { ctx = (init (), init ()); runtimeError = false }

  let _init () =
    _halt := false;
    _ctx := { ctx = (init (), init ()); runtimeError = false }

  let _exec ruleset_variant instr =
    if !_ctx.runtimeError then -1
    else
      match step ruleset_variant !_ctx.ctx instr with
      | Some (ctx1, ctx2) ->
          let () = Pp.printf "@{<green>ctx1:@} %s\n" (layout_ifcCtx ctx1) in
          let () = Pp.printf "@{<green>ctx2:@} %s\n" (layout_ifcCtx ctx2) in
          _ctx := { ctx = (ctx1, ctx2); runtimeError = false };
          List.length ctx1.stack
      | None ->
          _ctx := { ctx = !_ctx.ctx; runtimeError = true };
          -1

  let _check_enni () =
    if !_ctx.runtimeError then true
    else
      let ctx1, ctx2 = !_ctx.ctx in
      let () = Pp.printf "@{<green>success@}\n" in
      let () = Pp.printf "@{<green>ctx1:@} %s\n" (layout_ifcCtx ctx1) in
      let () = Pp.printf "@{<green>ctx2:@} %s\n" (layout_ifcCtx ctx2) in
      let res = equal_ifcCtx ctx1 ctx2 in
      let () =
        if res then Pp.printf "@{<green>res:@} %b\n" res
        else Pp.printf "@{<red>res:@} %b\n" res
      in
      res
end

open BasicIFC

let test_store_ab = [ Push (PublicV 1); Push (PrivateV (0, 1)); Store; Halt ]
let test_store_b = [ Push (PublicV 0); Push (PrivateV (0, 1)); Store; Halt ]

let test_add =
  [
    Push (PrivateV (0, 1)); Push (PublicV 0); Add; Push (PublicV 0); Store; Halt;
  ]

let test_load =
  [
    Push (PublicV 0);
    Push (PublicV 1);
    Push (PublicV 0);
    Store;
    Push (PrivateV (0, 1));
    Load;
    Store;
    Halt;
  ]

let test_store_ab_main () =
  let () = clear_instrs () in
  let () = tmp_instrs := test_store_ab in
  let res = check_eeni ruleset_wrong_store_ab in
  Printf.printf "res: %b\n" res

let test_store_b_main () =
  let () = clear_instrs () in
  let () = tmp_instrs := test_store_b in
  let res = check_eeni ruleset_wrong_store_b in
  Printf.printf "res: %b\n" res

let test_add_main () =
  let () = clear_instrs () in
  let () = tmp_instrs := test_add in
  let res = check_eeni ruleset_wrong_add in
  Printf.printf "res: %b\n" res

let test_load_main () =
  let () = clear_instrs () in
  let () = tmp_instrs := test_load in
  let res = check_eeni ruleset_wrong_load in
  Printf.printf "res: %b\n" res

open Language
open Interpreter

let _ruleset = ref ruleset_wrong_store_ab
let set_ruleset_store () = _ruleset := ruleset_wrong_store_ab
let set_ruleset_add () = _ruleset := ruleset_wrong_add
let set_ruleset_load () = _ruleset := ruleset_wrong_load

let pushPublicHandler (msg : msg) =
  let x = match msg.ev.args with [ VConst (I x) ] -> x | _ -> _die [%here] in
  let depth = _exec !_ruleset (Push (PublicV x)) in
  send ("stackDepth", [ mk_value_int depth ])

let pushPrivateHandler (msg : msg) =
  let x, y =
    match msg.ev.args with
    | [ VConst (I x); VConst (I y) ] -> (x, y)
    | _ -> _die [%here]
  in
  let depth = _exec !_ruleset (Push (PrivateV (x, y))) in
  send ("stackDepth", [ mk_value_int depth ])

let pushHandler (msg : msg) =
  let lv, x, y =
    match msg.ev.args with
    | [ VConst (B lv); VConst (I x); VConst (I y) ] -> (lv, x, y)
    | _ -> _die [%here]
  in
  let data =
    if not lv then PrivateV (x, y)
    else if x == y then PublicV x
    else _die_with [%here] "runtime error: public values should be equal"
  in
  let depth = _exec !_ruleset (Push data) in
  send ("stackDepth", [ mk_value_int depth ])

let popHandler (_ : msg) =
  let depth = _exec !_ruleset Pop in
  send ("stackDepth", [ mk_value_int depth ])

let loadHandler (_ : msg) =
  let depth = _exec !_ruleset Load in
  send ("stackDepth", [ mk_value_int depth ])

let storeHandler (_ : msg) =
  let depth = _exec !_ruleset Store in
  send ("stackDepth", [ mk_value_int depth ])

let addHandler (_ : msg) =
  let depth = _exec !_ruleset Add in
  send ("stackDepth", [ mk_value_int depth ])

let noopHandler (_ : msg) =
  let depth = _exec !_ruleset Noop in
  send ("stackDepth", [ mk_value_int depth ])

let haltHandler (_ : msg) =
  let depth = _exec !_ruleset Halt in
  send ("stackDepth", [ mk_value_int depth ])

let enniReqHandler (_ : msg) =
  let res = _check_enni () in
  send ("enniResp", [ mk_value_bool res ])

let stackDepthHandler (_ : msg) = ()
let enniRespHandler (_ : msg) = ()

let init () =
  register_handler "push" pushHandler;
  register_handler "pushPublic" pushPublicHandler;
  register_handler "pushPrivate" pushPrivateHandler;
  register_handler "pop" popHandler;
  register_handler "load" loadHandler;
  register_handler "store" storeHandler;
  register_handler "add" addHandler;
  register_handler "noop" noopHandler;
  register_handler "halt" haltHandler;
  register_handler "enniReq" enniReqHandler;
  register_handler "stackDepth" stackDepthHandler;
  register_handler "enniResp" enniRespHandler;
  _init ()

let trace_enni trace =
  let rec check = function
    | [] -> true
    | { ev = { op = "enniResp"; args = [ VConst (B x) ] }; _ } :: _ -> x
    | { ev = { op = "stackDepth"; args = [ VConst (I x) ] }; _ } :: rest ->
        if x < 0 then true else check rest
    | _ :: rest -> check rest
  in
  let res = check trace in
  res

type ifc_bench_config = { numOp : int }

let parse_config config =
  let numOp = get_config_value config "numOp" in
  { numOp }

let instrGen config =
  let { numOp } = parse_config config in
  let open QCheck.Gen in
  let valueGen = small_int in
  let random_push =
    oneof
      [
        map (fun x -> Push (PublicV x)) valueGen;
        map2 (fun x y -> Push (PrivateV (x, y))) valueGen valueGen;
      ]
  in
  let random_pop = pure Pop in
  let random_load = pure Load in
  let random_store = pure Store in
  let random_add = pure Add in
  let random_noop = pure Noop in
  let random_halt = pure Halt in
  let random_instr =
    oneof
      [
        random_push;
        random_pop;
        random_load;
        random_store;
        random_add;
        random_noop;
        random_halt;
      ]
  in
  sized_size (int_bound numOp)
  @@ fix (fun gen n ->
         match n with
         | 0 -> pure []
         | n ->
             oneof
               [
                 map (fun x -> [ x ]) random_halt;
                 map2 (fun x y -> x :: y) random_instr (gen (n - 1));
               ])

let _store = ref []

let _next conf =
  match !_store with
  | [] -> (
      let res = QCheck.Gen.generate ~n:10000 (instrGen conf) in
      match res with
      | [] -> _die_with [%here] "never"
      | e :: es ->
          _store := es;
          e)
  | e :: es ->
      _store := es;
      e

let exec_instrs e =
  let aux e =
    match e with
    | Push (PublicV x) -> send ("pushPublic", [ mk_value_int x ])
    | Push (PrivateV (x, y)) ->
        send ("pushPrivate", [ mk_value_int x; mk_value_int y ])
    | Pop -> send ("pop", [])
    | Load -> send ("load", [])
    | Store -> send ("store", [])
    | Add -> send ("add", [])
    | Noop -> send ("noop", [])
    | Halt -> send ("halt", [])
  in
  List.iter aux e;
  send ("enniReq", []);
  Effect.perform End

let randomTest config =
  let e = _next config in
  let () = Pp.printf "@{<green>instrs@} %s\n" (layout_instrs e) in
  exec_instrs e

let test_env =
  {
    if_concurrent = false;
    database_ctx = None;
    init_test_env = init;
    default_test_prog = [];
    property = trace_enni;
    random_test_gen = randomTest;
  }
