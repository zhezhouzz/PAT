open Database
open Language
open Interpreter

module MyDB (C : Config) = struct
  include MakeBD (C)

  let event_typectx =
    let open Nt in
    [
      "get"#:(Nt.mk_record None
                ([
                   "tid"#:int_ty;
                   "x"#:int_ty;
                   "prevTid"#:int_ty;
                   "prevCid"#:int_ty;
                 ]
                @ value_tys));
      "put"#:(Nt.mk_record None ([ "tid"#:int_ty; "x"#:int_ty ] @ value_tys));
      "beginT"#:(Nt.mk_record None [ "tid"#:int_ty ]);
      "commit"#:(Nt.mk_record None [ "tid"#:int_ty; "cid"#:int_ty ]);
    ]

  let msg_to_operation msg =
    let mk_put (k, v) =
      Some { opKind = Write; key = k; value = v; oid = { tid = 0; pid = 0 } }
    in
    let mk_get (k, v) =
      Some { opKind = Read; key = k; value = v; oid = { tid = 0; pid = 0 } }
    in
    match msg.ev.op with
    | "put" -> (
        match msg.ev.args with
        | _ :: VConst (I x) :: v -> mk_put (x, args_to_value v)
        | _ -> _die [%here])
    | "get" -> (
        match msg.ev.args with
        | _ :: VConst (I x) :: _ :: _ :: v -> mk_get (x, args_to_value v)
        | _ -> _die [%here])
    | _ -> None

  let serializable_trace_checker his =
    let his = List.filter_map msg_to_operation his in
    serializable_trace_check his

  let do_get tid x =
    let msg = async ("get", [ mk_value_int tid; mk_value_int x ]) in
    match msg.ev.args with
    | _ :: _ :: _ :: _ :: args -> args_to_value args
    | _ -> _die [%here]

  let do_put tid x v =
    async ("put", [ mk_value_int tid; mk_value_int x ] @ value_to_args v)

  let do_trans f =
    let msg = async ("beginT", []) in
    let tid =
      match msg.ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let res = f tid in
    let _ = async ("commit", [ mk_value_int tid ]) in
    res

  let beginAsync (ev : ev) =
    let tid = begin_transaction !Runtime._curTid in
    { ev with args = [ mk_value_int tid ] }

  let commitAsync (ev : ev) =
    let tid =
      match ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let cid = commit_transaction !Runtime._curTid tid in
    { ev with args = [ mk_value_int tid; mk_value_int cid ] }

  let getAsync (ev : ev) =
    let tid, x =
      match ev.args with
      | [ VConst (I tid); VConst (I x) ] -> (tid, x)
      | _ -> _die [%here]
    in
    let prev_tid, prev_cid, value = get !Runtime._curTid x in
    {
      ev with
      args =
        [
          mk_value_int tid;
          mk_value_int x;
          mk_value_int prev_tid;
          mk_value_int prev_cid;
        ]
        @ value_to_args value;
    }

  let putAsync (ev : ev) =
    let _, x, v =
      match ev.args with
      | VConst (I tid) :: VConst (I x) :: v -> (tid, x, args_to_value v)
      | _ -> _die [%here]
    in
    put !Runtime._curTid x v
end

module IntDB = struct
  include MyDB (struct
    type value = int

    let args_to_value args =
      match args with [ VConst (I y) ] -> y | _ -> _die [%here]

    let value_to_args y = [ mk_value_int y ]
    let equal_value x y = x == y
    let value_tys = Nt.[ "y"#:int_ty ]
    let initial_value = 0
    let layout_value x = spf "%i" x
  end)

  let event_typectx =
    let open Nt in
    event_typectx
    @ [
        "selectName"#:(Nt.mk_record None
                         [
                           "tid"#:int_ty;
                           "id"#:int_ty;
                           "prevTid"#:int_ty;
                           "prevCid"#:int_ty;
                           "name"#:int_ty;
                         ]);
        "updateName"#:(Nt.mk_record None
                         [ "tid"#:int_ty; "id"#:int_ty; "name"#:int_ty ]);
        "selectSaving"#:(Nt.mk_record None
                           [
                             "tid"#:int_ty;
                             "id"#:int_ty;
                             "prevTid"#:int_ty;
                             "prevCid"#:int_ty;
                             "savings"#:int_ty;
                           ]);
        "updateSaving"#:(Nt.mk_record None
                           [ "tid"#:int_ty; "id"#:int_ty; "savings"#:int_ty ]);
        "selectChecking"#:(Nt.mk_record None
                             [
                               "tid"#:int_ty;
                               "id"#:int_ty;
                               "prevTid"#:int_ty;
                               "prevCid"#:int_ty;
                               "checking"#:int_ty;
                             ]);
        "updateChecking"#:(Nt.mk_record None
                             [ "tid"#:int_ty; "id"#:int_ty; "checking"#:int_ty ]);
      ]

  let interval = 100

  let wr_to_put_get msg =
    match msg.ev.op with
    | "selectName" -> (
        match msg.ev.args with
        | [
         VConst (I tid);
         VConst (I id);
         VConst (I prev_tid);
         VConst (I prev_cid);
         VConst (I name);
        ] ->
            let ev =
              {
                op = "put";
                args =
                  [
                    mk_value_int tid;
                    mk_value_int id;
                    mk_value_int prev_tid;
                    mk_value_int prev_cid;
                    mk_value_int name;
                  ];
              }
            in
            { msg with ev }
        | _ -> _die [%here])
    | "updateName" -> (
        match msg.ev.args with
        | [ VConst (I tid); VConst (I id); VConst (I name) ] ->
            let ev =
              {
                op = "put";
                args = [ mk_value_int tid; mk_value_int id; mk_value_int name ];
              }
            in
            { msg with ev }
        | _ -> _die [%here])
    | "selectSaving" -> (
        match msg.ev.args with
        | [
         VConst (I tid);
         VConst (I id);
         VConst (I prev_tid);
         VConst (I prev_cid);
         VConst (I savings);
        ] ->
            let ev =
              {
                op = "get";
                args =
                  [
                    mk_value_int tid;
                    mk_value_int (id + interval);
                    mk_value_int prev_tid;
                    mk_value_int prev_cid;
                    mk_value_int savings;
                  ];
              }
            in
            { msg with ev }
        | _ -> _die [%here])
    | "updateSaving" -> (
        match msg.ev.args with
        | [ VConst (I tid); VConst (I id); VConst (I savings) ] ->
            let ev =
              {
                op = "put";
                args =
                  [
                    mk_value_int tid;
                    mk_value_int (id + interval);
                    mk_value_int savings;
                  ];
              }
            in
            { msg with ev }
        | _ -> _die [%here])
    | "selectChecking" -> (
        match msg.ev.args with
        | [
         VConst (I tid);
         VConst (I id);
         VConst (I prev_tid);
         VConst (I prev_cid);
         VConst (I checking);
        ] ->
            let ev =
              {
                op = "get";
                args =
                  [
                    mk_value_int tid;
                    mk_value_int (id + (2 * interval));
                    mk_value_int prev_tid;
                    mk_value_int prev_cid;
                    mk_value_int checking;
                  ];
              }
            in
            { msg with ev }
        | _ -> _die [%here])
    | "updateChecking" -> (
        match msg.ev.args with
        | [ VConst (I tid); VConst (I id); VConst (I checking) ] ->
            let ev =
              {
                op = "put";
                args =
                  [
                    mk_value_int tid;
                    mk_value_int (id + (2 * interval));
                    mk_value_int checking;
                  ];
              }
            in
            { msg with ev }
        | _ -> _die [%here])
    | _ -> msg

  let serializable_trace_checker his =
    let his = List.map wr_to_put_get his in
    serializable_trace_checker his

  let do_selectName tid id =
    let msg = async ("selectName", [ mk_value_int tid; mk_value_int id ]) in
    match msg.ev.args with
    | [ VConst (I _); VConst (I _); VConst (I _); VConst (I _); VConst (I y) ]
      ->
        y
    | _ -> _die [%here]

  let do_updateName tid id name =
    async
      ("updateName", [ mk_value_int tid; mk_value_int id; mk_value_int name ])

  let do_selectSaving tid id =
    let msg = async ("selectSaving", [ mk_value_int tid; mk_value_int id ]) in
    match msg.ev.args with
    | [ VConst (I _); VConst (I _); VConst (I _); VConst (I _); VConst (I y) ]
      ->
        y
    | _ -> _die [%here]

  let do_updateSaving tid id savings =
    async
      ( "updateSaving",
        [ mk_value_int tid; mk_value_int id; mk_value_int savings ] )

  let do_selectChecking tid id =
    let msg = async ("selectChecking", [ mk_value_int tid; mk_value_int id ]) in
    match msg.ev.args with
    | [ VConst (I _); VConst (I _); VConst (I _); VConst (I _); VConst (I y) ]
      ->
        y
    | _ -> _die [%here]

  let do_updateChecking tid id checking =
    async
      ( "updateChecking",
        [ mk_value_int tid; mk_value_int id; mk_value_int checking ] )

  let selectNameAsync (ev : ev) =
    let tid, id =
      match ev.args with
      | [ VConst (I tid); VConst (I id) ] -> (tid, id)
      | _ -> _die [%here]
    in
    let prev_tid, prev_cid, y = get !Runtime._curTid id in
    {
      ev with
      args =
        [
          mk_value_int tid;
          mk_value_int id;
          mk_value_int prev_tid;
          mk_value_int prev_cid;
          mk_value_int y;
        ];
    }

  let updateNameAsync (ev : ev) =
    let _, id, name =
      match ev.args with
      | [ VConst (I tid); VConst (I id); VConst (I name) ] -> (tid, id, name)
      | _ -> _die [%here]
    in
    put !Runtime._curTid id name

  let selectSavingAsync (ev : ev) =
    let tid, id =
      match ev.args with
      | [ VConst (I tid); VConst (I id) ] -> (tid, id)
      | _ -> _die [%here]
    in
    let prev_tid, prev_cid, y = get !Runtime._curTid (id + interval) in
    {
      ev with
      args =
        [
          mk_value_int tid;
          mk_value_int id;
          mk_value_int prev_tid;
          mk_value_int prev_cid;
          mk_value_int y;
        ];
    }

  let updateSavingAsync (ev : ev) =
    let _, id, savings =
      match ev.args with
      | [ VConst (I tid); VConst (I id); VConst (I savings) ] ->
          (tid, id, savings)
      | _ -> _die [%here]
    in
    put !Runtime._curTid (id + interval) savings

  let selectCheckingAsync (ev : ev) =
    let tid, id =
      match ev.args with
      | [ VConst (I tid); VConst (I id) ] -> (tid, id)
      | _ -> _die [%here]
    in
    let prev_tid, prev_cid, y = get !Runtime._curTid (id + (2 * interval)) in
    {
      ev with
      args =
        [
          mk_value_int tid;
          mk_value_int id;
          mk_value_int prev_tid;
          mk_value_int prev_cid;
          mk_value_int y;
        ];
    }

  let updateCheckingAsync (ev : ev) =
    let _, id, checking =
      match ev.args with
      | [ VConst (I tid); VConst (I id); VConst (I checking) ] ->
          (tid, id, checking)
      | _ -> _die [%here]
    in
    put !Runtime._curTid (id + (2 * interval)) checking
end

module PairDB = struct
  include MyDB (struct
    type value = int * int

    let args_to_value args =
      match args with
      | [ VConst (I y); VConst (I z) ] -> (y, z)
      | _ -> _die [%here]

    let value_to_args (y, z) = [ mk_value_int y; mk_value_int z ]
    let equal_value (x, y) (x', y') = x == x' && y == y'
    let value_tys = Nt.[ "y"#:int_ty; "z"#:int_ty ]
    let initial_value = (0, 0)
    let layout_value (x, y) = spf "(%i, %i)" x y
  end)

  let event_typectx =
    let open Nt in
    event_typectx
    @ [
        "read"#:(Nt.mk_record None
                   [
                     "tid"#:int_ty;
                     "prevTid"#:int_ty;
                     "prevCid"#:int_ty;
                     "y"#:int_ty;
                   ]);
        "write"#:(Nt.mk_record None [ "tid"#:int_ty; "y"#:int_ty ]);
      ]

  let wr_to_put_get msg =
    match msg.ev.op with
    | "write" -> (
        match msg.ev.args with
        | [ VConst (I tid); VConst (I y) ] ->
            let ev =
              {
                op = "put";
                args =
                  [
                    mk_value_int tid;
                    mk_value_int predefined_key;
                    mk_value_int y;
                    mk_value_int y;
                  ];
              }
            in
            { msg with ev }
        | _ -> _die [%here])
    | "read" -> (
        match msg.ev.args with
        | [
         VConst (I tid); VConst (I prev_tid); VConst (I prev_cid); VConst (I y);
        ] ->
            let ev =
              {
                op = "get";
                args =
                  [
                    mk_value_int tid;
                    mk_value_int predefined_key;
                    mk_value_int prev_tid;
                    mk_value_int prev_cid;
                    mk_value_int y;
                    mk_value_int y;
                  ];
              }
            in
            { msg with ev }
        | _ -> _die [%here])
    | _ -> msg

  let serializable_trace_checker his =
    let his = List.map wr_to_put_get his in
    serializable_trace_checker his

  let do_read tid =
    let msg = async ("read", [ mk_value_int tid ]) in
    match msg.ev.args with
    | [ VConst (I _); VConst (I _); VConst (I _); VConst (I y) ] -> y
    | _ -> _die [%here]

  let do_write tid x = async ("write", [ mk_value_int tid; mk_value_int x ])

  let readAsync (ev : ev) =
    let tid =
      match ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let prev_tid, prev_cid, (_, y) = read !Runtime._curTid in
    {
      ev with
      args =
        [
          mk_value_int tid;
          mk_value_int prev_tid;
          mk_value_int prev_cid;
          mk_value_int y;
        ];
    }

  let writeAsync (ev : ev) =
    let x =
      match ev.args with
      | [ VConst (I _); VConst (I x) ] -> x
      | _ -> _die [%here]
    in
    write !Runtime._curTid (x, x)
end

module ListDB = struct
  include MyDB (struct
    type value = int list

    let equal_value l l' = List.equal ( == ) l l'
    let value_tys = Nt.[ "y"#:(mk_list_ty int_ty) ]
    let initial_value = []

    let args_to_value args =
      match args with [ VCIntList y ] -> y | _ -> _die [%here]

    let value_to_args y = [ mk_value_intList y ]

    let layout_value l =
      spf "[%s]" (String.concat ", " (List.map string_of_int l))
  end)

  let event_typectx =
    let open Nt in
    event_typectx
    @ [
        "read"#:(Nt.mk_record None
                   ([ "tid"#:int_ty; "prevTid"#:int_ty; "prevCid"#:int_ty ]
                   @ value_tys));
        "write"#:(Nt.mk_record None ([ "tid"#:int_ty ] @ value_tys));
      ]

  let wr_to_put_get msg =
    match msg.ev.op with
    | "write" -> (
        match msg.ev.args with
        | [ VConst (I tid); VCIntList y ] ->
            let ev =
              {
                op = "put";
                args =
                  [
                    mk_value_int tid;
                    mk_value_int predefined_key;
                    mk_value_intList y;
                  ];
              }
            in
            { msg with ev }
        | _ -> _die [%here])
    | "read" -> (
        match msg.ev.args with
        | [
         VConst (I tid); VConst (I prev_tid); VConst (I prev_cid); VCIntList y;
        ] ->
            let ev =
              {
                op = "get";
                args =
                  [
                    mk_value_int tid;
                    mk_value_int predefined_key;
                    mk_value_int prev_tid;
                    mk_value_int prev_cid;
                    mk_value_intList y;
                  ];
              }
            in
            { msg with ev }
        | _ -> _die [%here])
    | _ -> msg

  let serializable_trace_checker his =
    let his = List.map wr_to_put_get his in
    serializable_trace_checker his

  let do_read tid =
    let msg = async ("read", [ mk_value_int tid ]) in
    match msg.ev.args with
    | [ VConst (I _); VConst (I _); VConst (I _); VCIntList y ] -> y
    | _ -> _die [%here]

  let do_write tid x = async ("write", [ mk_value_int tid; mk_value_intList x ])

  let readAsync (ev : ev) =
    let tid =
      match ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let prev_tid, prev_cid, y = read !Runtime._curTid in
    {
      ev with
      args =
        [
          mk_value_int tid;
          mk_value_int prev_tid;
          mk_value_int prev_cid;
          mk_value_intList y;
        ];
    }

  let writeAsync (ev : ev) =
    let x =
      match ev.args with
      | [ VConst (I _); VCIntList x ] -> x
      | _ -> _die [%here]
    in
    write !Runtime._curTid x
end
