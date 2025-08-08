open Language
open Zdatatype

type isolation = Serializable | Causal | ReadCommitted

module type Config = sig
  type value

  val initial_value : value
  val layout_value : value -> string
end

module MakeBD (Config : Config) = struct
  open Config

  type key = int
  type oid = { tid : int; pid : int } [@@deriving show, eq, ord]
  type opKind = Read | Write
  type operation = { opKind : opKind; key : key; value : value; oid : oid }
  type transaction = { id : int; operations : (int, operation) Hashtbl.t }
  type so = (int, int) Hashtbl.t (* session order: a partial order *)
  type wr = (oid, oid) Hashtbl.t
  (* inverser of write-read relation: from read oid to corresponding write oid and key*)

  type co = int list (* commit order: a total order *)

  let initial_tid = 0
  let initial_value = (0, 0)

  type database = {
    history : (int, transaction) Hashtbl.t;
    committed : (int, unit) Hashtbl.t;
    so : so;
    wr : wr;
    co : co;
    threadStatus : (int, int) Hashtbl.t;
    isolation : isolation;
  }

  let _init isolation =
    {
      history = Hashtbl.create 10;
      committed = Hashtbl.create 10;
      so = Hashtbl.create 10;
      wr = Hashtbl.create 10;
      co = [];
      threadStatus = Hashtbl.create 10;
      isolation;
    }

  let database = ref (_init Serializable)
  let init isolation = database := _init isolation

  let count_transactions () =
    let tid = Hashtbl.length !database.history in
    tid + 1

  let new_tid () = count_transactions ()

  let calculate_wr () =
    let wr =
      List.of_seq
        (Seq.map
           (fun (read_oid, write_oid) -> (write_oid.tid, read_oid.tid))
           (Hashtbl.to_seq !database.wr))
    in
    let wr =
      List.sort_uniq
        (fun (x1, x2) (y1, y2) ->
          if Int.compare x1 y1 == 0 then Int.compare x2 y2
          else Int.compare x1 y1)
        wr
    in
    wr

  let calculate_so () =
    let so =
      List.of_seq
        (Seq.map
           (fun (tid1, tid2) -> (tid1, tid2))
           (Hashtbl.to_seq !database.so))
    in
    let so' =
      List.init (count_transactions () - 1) (fun i -> (initial_tid, i + 1))
    in
    so @ so'

  let make_closure mat =
    let size = Array.length mat in
    let changed = ref true in
    while !changed do
      changed := false;
      for i = 0 to size - 1 do
        for j = 0 to size - 1 do
          if mat.(i).(j) then
            for k = 0 to size - 1 do
              if mat.(j).(k) then mat.(i).(k) <- true;
              changed := true
            done
        done
      done
    done

  let check_partial_order mat =
    let size = Array.length mat in
    let res = ref true in
    for i = 0 to size - 1 do
      for j = 0 to size - 1 do
        if mat.(i).(j) && mat.(j).(i) then res := false
      done
    done;
    !res

  let check_causal (wr, so) =
    let size = Hashtbl.length !database.history in
    let mat = Array.init size (fun _ -> Array.make size false) in
    let () =
      List.iter (fun (tid1, tid2) -> mat.(tid1).(tid2) <- true) (wr @ so)
    in
    let () = make_closure mat in
    check_partial_order mat

  exception IsolationViolation of string

  let begin_transaction thread_id =
    let () =
      match !database.isolation with
      | Serializable ->
          if
            Hashtbl.length !database.history
            != Hashtbl.length !database.committed
          then
            raise
              (IsolationViolation "SC violation: not all transactions committed")
      | _ -> ()
    in
    let new_tid = new_tid () in
    let new_transaction = { id = new_tid; operations = Hashtbl.create 10 } in
    (* Add SO *)
    let () =
      match Hashtbl.find_opt !database.threadStatus thread_id with
      | Some tid -> Hashtbl.add !database.so tid new_tid
      | None -> ()
    in
    let () = Hashtbl.add !database.threadStatus thread_id new_tid in
    let () = Hashtbl.add !database.history new_tid new_transaction in
    let () =
      match !database.isolation with
      | Serializable ->
          database := { !database with co = !database.co @ [ new_tid ] }
      | _ -> ()
    in
    let () =
      Pp.printf "thread (%i) begin_transaction: %i\n" thread_id new_tid
    in
    new_tid

  let commit_transaction thread_id cur_tid =
    let () =
      Pp.printf "thread (%i) commit_transaction: %i\n" thread_id cur_tid
    in
    match Hashtbl.find_opt !database.threadStatus thread_id with
    | Some tid -> (
        let () = assert (tid == cur_tid) in
        Hashtbl.add !database.committed tid ();
        match !database.isolation with
        | ReadCommitted ->
            database := { !database with co = !database.co @ [ tid ] }
        | _ -> ())
    | None ->
        _die_with [%here] (spf "thread %i is not in a transaction" thread_id)

  let get_transaction_from_thread thread_id =
    match Hashtbl.find_opt !database.threadStatus thread_id with
    | Some tid -> tid
    | None ->
        _die_with [%here] (spf "thread %i is not in a transaction" thread_id)

  let new_oid tid =
    let trans = Hashtbl.find !database.history tid in
    let pid = Hashtbl.length trans.operations in
    { tid; pid }

  let add_operation tid operation =
    let trans = Hashtbl.find !database.history tid in
    Hashtbl.add trans.operations operation.oid.pid operation

  let all_committed_transactions_in_history () =
    Hashtbl.fold
      (fun tid _ acc ->
        let trans = Hashtbl.find !database.history tid in
        trans :: acc)
      !database.committed []

  let get_operations_from_trans trans =
    List.init (Hashtbl.length trans.operations) (fun i ->
        Hashtbl.find trans.operations i)

  let last_writes_in_transaction trans =
    let operations = get_operations_from_trans trans in
    let res =
      List.fold_left
        (fun acc operation ->
          match operation.opKind with
          | Read -> acc
          | Write -> IntMap.update operation.key (fun _ -> Some operation) acc)
        IntMap.empty operations
    in
    (trans.id, res)

  let try_read_from_transaction tid key =
    let trans = Hashtbl.find !database.history tid in
    let _, last_write = last_writes_in_transaction trans in
    match IntMap.find_opt last_write key with
    | Some { value; _ } -> Some value
    | _ -> None

  let all_last_writes_in_history () =
    let transactions = all_committed_transactions_in_history () in
    let last_writes = List.map last_writes_in_transaction transactions in
    last_writes

  let all_committed_operations_in_history () =
    let last_writes = all_last_writes_in_history () in
    let operations =
      List.map
        (fun (_, last_write) ->
          List.map snd @@ List.of_seq (IntMap.to_seq last_write))
        last_writes
    in
    List.concat operations

  let all_keys_in_history () =
    let operations = all_committed_operations_in_history () in
    let keys = List.map (fun operation -> operation.key) operations in
    List.sort_uniq Int.compare keys

  let put thread_id key value =
    let tid = get_transaction_from_thread thread_id in
    let oid = new_oid tid in
    let operation = { opKind = Write; key; value; oid } in
    let () = add_operation tid operation in
    ()

  let layout_operation_kind opKind =
    match opKind with Read -> "Read" | Write -> "Write"

  let layout_operation operation =
    spf "%s(%i, %s)"
      (layout_operation_kind operation.opKind)
      operation.key
      (layout_value operation.value)

  let layout_last_write (tid, m) =
    IntMap.fold
      (fun key operation acc ->
        spf "%skey: %i, value: %s\n" acc key (layout_operation operation))
      m (spf "tid: %i\n" tid)

  let get thread_id key =
    let tid = get_transaction_from_thread thread_id in
    let oid = new_oid tid in
    let transaction = Hashtbl.find !database.history tid in
    let _, last_write = last_writes_in_transaction transaction in
    let value =
      match IntMap.find_opt last_write key with
      | Some { value; _ } -> value
      | _ -> (
          match !database.isolation with
          | ReadCommitted -> (
              let () =
                Pp.printf "read from transactions: %s\n"
                  (String.concat ", " (List.map string_of_int !database.co))
              in
              let last_writes = all_last_writes_in_history () in
              let () =
                Pp.printf "last_writes: %s\n"
                  (String.concat ", " (List.map layout_last_write last_writes))
              in
              let value =
                List.fold_right
                  (fun tid acc ->
                    match acc with
                    | Some value -> Some value
                    | None -> (
                        match try_read_from_transaction tid key with
                        | Some value -> Some value
                        | _ -> acc))
                  !database.co None
              in
              match value with
              | None -> Config.initial_value
              | Some value -> value)
          | Serializable -> (
              let co =
                List.filter
                  (fun tid -> Hashtbl.mem !database.committed tid)
                  !database.co
              in
              let value =
                List.fold_right
                  (fun tid acc ->
                    match acc with
                    | Some value -> Some value
                    | None -> (
                        match try_read_from_transaction tid key with
                        | Some value -> Some value
                        | _ -> acc))
                  co None
              in
              match value with
              | None -> Config.initial_value
              | Some value -> value)
          | Causal ->
              let last_writes = all_committed_operations_in_history () in
              let last_writes =
                List.filter (fun operation -> operation.key == key) last_writes
              in
              let wr = calculate_wr () in
              let so = calculate_so () in
              let values =
                List.filter
                  (fun operation ->
                    let wr = (operation.oid.tid, tid) :: wr in
                    check_causal (wr, so))
                  last_writes
              in
              let value =
                if List.length values == 0 then Config.initial_value
                else
                  let operation =
                    List.nth values (Random.int (List.length values))
                  in
                  let () = Hashtbl.add !database.wr oid operation.oid in
                  operation.value
              in
              value)
    in
    let operation = { opKind = Read; key; value; oid } in
    let () = add_operation tid operation in
    operation.value

  let predefined_key = 5555
  let write thread_id value = put thread_id predefined_key value
  let read thread_id = get thread_id predefined_key
end
