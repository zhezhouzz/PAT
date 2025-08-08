open Database
open Language
open Interpreter

module StackDB = struct
  include MakeBD (struct
    type value = int * int

    let initial_value = (0, 0)
    let layout_value (x, y) = spf "(%i, %i)" x y
  end)

  let event_typectx =
    let open Nt in
    [
      "get"#:(Nt.mk_record None
                [ "tid"#:int_ty; "x"#:int_ty; "y"#:int_ty; "z"#:int_ty ]);
      "put"#:(Nt.mk_record None
                [ "tid"#:int_ty; "x"#:int_ty; "y"#:int_ty; "z"#:int_ty ]);
      "read"#:(Nt.mk_record None [ "tid"#:int_ty; "x"#:int_ty; "y"#:int_ty ]);
      "write"#:(Nt.mk_record None [ "tid"#:int_ty; "x"#:int_ty; "y"#:int_ty ]);
      "begin"#:(Nt.mk_record None [ "tid"#:int_ty ]);
      "commit"#:(Nt.mk_record None [ "tid"#:int_ty ]);
    ]

  let do_read tid =
    let msg = async ("read", [ mk_value_int tid ]) in
    match msg.ev.args with
    | [ VConst (I _); VConst (I y) ] -> y
    | _ -> _die [%here]

  let do_write tid x = async ("write", [ mk_value_int tid; mk_value_int x ])

  let do_get tid x =
    let msg = async ("get", [ mk_value_int tid; mk_value_int x ]) in
    match msg.ev.args with
    | [ VConst (I _); VConst (I _); VConst (I y); VConst (I z) ] -> (y, z)
    | _ -> _die [%here]

  let do_put tid x (y, z) =
    async
      ( "put",
        [ mk_value_int tid; mk_value_int x; mk_value_int y; mk_value_int z ] )

  let do_trans f =
    let msg = async ("begin", []) in
    let tid =
      match msg.ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let _ = f tid in
    let _ = async ("commit", [ mk_value_int tid ]) in
    ()

  let beginAsync (ev : ev) =
    let tid = begin_transaction !Runtime._curTid in
    { ev with args = [ mk_value_int tid ] }

  let commitAsync (ev : ev) =
    let tid =
      match ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let _ = commit_transaction !Runtime._curTid tid in
    ev

  let getAsync (ev : ev) =
    let tid, x =
      match ev.args with
      | [ VConst (I tid); VConst (I x) ] -> (tid, x)
      | _ -> _die [%here]
    in
    let content, next = get !Runtime._curTid x in
    {
      ev with
      args =
        [
          mk_value_int tid;
          mk_value_int x;
          mk_value_int content;
          mk_value_int next;
        ];
    }

  let putAsync (ev : ev) =
    let _, x, y, z =
      match ev.args with
      | [ VConst (I tid); VConst (I x); VConst (I y); VConst (I z) ] ->
          (tid, x, y, z)
      | _ -> _die [%here]
    in
    let _ = put !Runtime._curTid x (y, z) in
    ev

  let readAsync (ev : ev) =
    let tid =
      match ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let _, y = read !Runtime._curTid in
    { ev with args = [ mk_value_int tid; mk_value_int y ] }

  let writeAsync (ev : ev) =
    let x =
      match ev.args with
      | [ VConst (I _); VConst (I x) ] -> x
      | _ -> _die [%here]
    in
    let _ = write !Runtime._curTid (x, x) in
    ev
end

module CartDB = struct
  include MakeBD (struct
    type value = int list

    let initial_value = []

    let layout_value l =
      spf "[%s]" (String.concat ", " (List.map string_of_int l))
  end)

  let event_typectx =
    let open Nt in
    [
      "get"#:(Nt.mk_record None
                [ "tid"#:int_ty; "x"#:int_ty; "y"#:(mk_list_ty int_ty) ]);
      "put"#:(Nt.mk_record None
                [ "tid"#:int_ty; "x"#:int_ty; "y"#:(mk_list_ty int_ty) ]);
      "begin"#:(Nt.mk_record None [ "tid"#:int_ty ]);
      "commit"#:(Nt.mk_record None [ "tid"#:int_ty ]);
    ]

  let do_get tid x =
    let msg = async ("get", [ mk_value_int tid; mk_value_int x ]) in
    match msg.ev.args with
    | [ VConst (I _); VConst (I _); VCIntList y ] -> y
    | _ -> _die [%here]

  let do_put tid x y =
    async ("put", [ mk_value_int tid; mk_value_int x; mk_value_intList y ])

  let do_trans f =
    let msg = async ("begin", []) in
    let tid =
      match msg.ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    let _ = f tid in
    let _ = async ("commit", [ mk_value_int tid ]) in
    ()

  let beginAsync (ev : ev) =
    let tid = begin_transaction !Runtime._curTid in
    { ev with args = [ mk_value_int tid ] }

  let commitAsync (ev : ev) =
    let tid =
      match ev.args with [ VConst (I tid) ] -> tid | _ -> _die [%here]
    in
    commit_transaction !Runtime._curTid tid

  let getAsync (ev : ev) =
    let tid, x =
      match ev.args with
      | [ VConst (I tid); VConst (I x) ] -> (tid, x)
      | _ -> _die [%here]
    in
    let l = get !Runtime._curTid x in
    { ev with args = [ mk_value_int tid; mk_value_int x; mk_value_intList l ] }

  let putAsync (ev : ev) =
    let _, x, y =
      match ev.args with
      | [ VConst (I tid); VConst (I x); VCIntList y ] -> (tid, x, y)
      | _ -> _die [%here]
    in
    put !Runtime._curTid x y
end
