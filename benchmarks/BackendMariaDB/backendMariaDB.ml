open Lwt.Infix
open Printf
module S = Mariadb.Nonblocking.Status

exception DBKeyNotFound of string

module M = Mariadb.Nonblocking.Make (struct
  module IO = struct
    type 'a future = 'a Lwt.t

    let ( >>= ) = ( >>= )
    let return = Lwt.return
  end

  let wait mariadb status =
    let fd = Lwt_unix.of_unix_file_descr @@ Mariadb.Nonblocking.fd mariadb in
    assert (S.read status || S.write status || S.timeout status);
    let idle, _ = Lwt.task () in
    let rt = if S.read status then Lwt_unix.wait_read fd else idle in
    let wt = if S.write status then Lwt_unix.wait_write fd else idle in
    let tt =
      match (S.timeout status, Mariadb.Nonblocking.timeout mariadb) with
      | true, 0 -> Lwt.return ()
      | true, tmout -> Lwt_unix.timeout (float tmout)
      | false, _ -> idle
    in
    Lwt.catch
      (fun () ->
        Lwt.nchoose [ rt; wt; tt ] >>= fun _ ->
        Lwt.return
        @@ S.create ~read:(Lwt_unix.readable fd) ~write:(Lwt_unix.writable fd)
             ())
      (function
        | Lwt_unix.Timeout -> Lwt.return @@ S.create ~timeout:true ()
        | e -> Lwt.fail e)
end)

let env var def = try Sys.getenv var with Not_found -> def

let or_die where = function
  | Ok r -> Lwt.return r
  | Error (i, e) -> Lwt.fail_with @@ sprintf "%s: (%d) %s" where i e

let connect port db_name =
  M.connect
    ~host:(env "OCAML_MARIADB_HOST" "127.0.0.1")
    ~user:(env "OCAML_MARIADB_USER" "root")
    ~port
    ~pass:(env "OCAML_MARIADB_PASS" "rootpass")
    ~db:(env "OCAML_MARIADB_DB" db_name)
    ()

let stream res =
  let next _ =
    M.Res.fetch (module M.Row.Map) res >>= function
    | Ok (Some _ as row) -> Lwt.return row
    | Ok None -> Lwt.return_none
    | Error _ -> Lwt.return_none
  in
  Lwt.return (Lwt_stream.from next)

let res_to_list res =
  let next _ =
    M.Res.fetch (module M.Row.Map) res >>= function
    | Ok (Some _ as row) -> Lwt.return row
    | Ok None -> Lwt.return_none
    | Error _ -> Lwt.return_none
  in
  Lwt_stream.to_list @@ Lwt_stream.from next

let print_row row =
  Lwt_io.printf "[DB] ---\n%!" >>= fun () ->
  M.Row.StringMap.fold
    (fun name field _ ->
      Lwt_io.printf "[DB] %20s " name >>= fun () ->
      match M.Field.value field with
      | `Int i -> Lwt_io.printf "%d\n%!" i
      | `Float x -> Lwt_io.printf "%f\n%!" x
      | `String s -> Lwt_io.printf "%s\n%!" s
      | `Bytes b -> Lwt_io.printf "%s\n%!" (Bytes.to_string b)
      | `Time t ->
          Lwt_io.printf "%04d-%02d-%02d %02d:%02d:%02d\n%!" (M.Time.year t)
            (M.Time.month t) (M.Time.day t) (M.Time.hour t) (M.Time.minute t)
            (M.Time.second t)
      | `Null -> Lwt_io.printf "NULL\n%!")
    row Lwt.return_unit

open Lwt.Syntax

let no_param_no_ret db query =
  let* stmt = M.prepare db query >>= or_die "prepare" in
  M.Stmt.execute stmt [||] >>= or_die "exec" >>= fun _ ->
  M.Stmt.close stmt >>= or_die "stmt close" >>= fun () -> Lwt.return_unit

module type MyDB = sig
  val maria_context : string -> Language.isolation -> (unit -> unit) -> unit
  val async_clear_db : unit -> unit Lwt.t
  val async_begin : thread_id:int -> unit -> int Lwt.t
  val async_commit : tid:int -> unit -> int Lwt.t
  val async_release_connection : tid:int -> unit -> unit Lwt.t

  val async_get :
    tid:int ->
    table:string ->
    key:string ->
    unit ->
    (int * int * Yojson.Basic.t) Lwt.t

  val async_put :
    tid:int ->
    table:string ->
    key:string ->
    json:Yojson.Basic.t ->
    unit ->
    unit Lwt.t

  val table_async_get :
    tid:int ->
    db:string ->
    table:string ->
    key:string ->
    unit ->
    (int * int * Yojson.Basic.t) Lwt.t

  val table_async_put :
    tid:int ->
    db:string ->
    table:string ->
    key:string ->
    json:Yojson.Basic.t ->
    unit ->
    unit Lwt.t

  val async_get_current_isolation : tid:int -> unit -> unit Lwt.t
  val raw_clear_db : unit -> unit
  val raw_begin : thread_id:int -> int
  val raw_commit : tid:int -> int

  val raw_get :
    tid:int -> table:string -> key:string -> int * int * Yojson.Basic.t

  val raw_put :
    tid:int -> table:string -> key:string -> json:Yojson.Basic.t -> unit

  val raw_get_current_isolation : tid:int -> unit
  val check_causal : unit -> bool
  val check_serializable : unit -> bool
  val check_read_committed : unit -> bool
  val layout_field : M.Field.t -> unit Lwt.t
end

module MyMariaDB : MyDB = struct
  (** Schema *)

  let _tid = ref 0
  let _cid = ref 0

  let next_tid () =
    let tid = !_tid in
    _tid := tid + 1;
    tid

  let next_cid () =
    let cid = !_cid in
    _cid := cid + 1;
    cid

  let commit_tid = Hashtbl.create 10

  let connectionPool : (int, M.t) Hashtbl.t =
    Hashtbl.create 10 (* max length 3 *)

  let connectionStatus = Hashtbl.create 3

  (* tid to connection id *)
  let connMap : (int, int) Hashtbl.t = Hashtbl.create 10

  let use_connection () =
    let connId =
      Hashtbl.fold
        (fun conn status res ->
          match res with
          | Some conn -> Some conn
          | None -> if status then Some conn else None)
        connectionStatus None
    in
    match connId with
    | Some id ->
        Hashtbl.replace connectionStatus id false;
        id
    | None -> Zutils.(_die_with [%here] "no connection available")

  (* let release_connection id = Hashtbl.replace connectionStatus id true *)

  type log =
    | Begin of { tid : int }
    | Commit of { tid : int; cid : int }
    | Put of { tid : int; key : string; value : Yojson.Basic.t }
    | Get of { tid : int; key : string; value : Yojson.Basic.t }

  let so : (int, int list) Hashtbl.t = Hashtbl.create 10
  let wr : (int * int) list ref = ref []
  let co : int list ref = ref []
  let _history : log list ref = ref []
  let _db_name = ref "cart"
  let _isolation = ref Language.Serializable
  let feild_name = "values_json"
  let wrapper_tid_json_feild = "tid"
  let wrapper_value_json_feild = "vv"

  let make_closure mat =
    let size = Array.length mat in
    let changed = ref false in
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

  let list_to_relation l =
    let _, res =
      List.fold_left
        (fun (prefix, res) j ->
          let r = List.map (fun i -> (i, j)) prefix in
          (prefix @ [ j ], res @ r))
        ([], []) l
    in
    res

  let add_relations mat rs =
    List.iter (fun (tid1, tid2) -> mat.(tid1).(tid2) <- true) rs

  let check_causal () =
    let size = !_tid in
    let mat = Array.init size (fun _ -> Array.make size false) in
    let () = add_relations mat !wr in
    let () = add_relations mat (list_to_relation !co) in
    let () =
      Hashtbl.iter (fun _ l -> add_relations mat (list_to_relation l)) so
    in
    let () = make_closure mat in
    check_partial_order mat

  let check_serializable () =
    let store = Hashtbl.create 10 in
    List.fold_left
      (fun res log ->
        if res then
          match log with
          | Begin _ | Commit _ -> res
          | Put { key; value; _ } -> (
              match Hashtbl.find_opt store key with
              | Some _ ->
                  Hashtbl.replace store key value;
                  res
              | None ->
                  Hashtbl.add store key value;
                  res)
          | Get { key; value; _ } -> (
              match Hashtbl.find_opt store key with
              | Some value' ->
                  if Yojson.Basic.equal value value' then res else false
              | None -> res)
        else false)
      true !_history

  let check_read_committed () =
    let open Zutils in
    let open Zdatatype in
    let store = Hashtbl.create 10 in
    let tmp = Hashtbl.create 10 in
    let apply_s s =
      StrMap.iter
        (fun key value ->
          match Hashtbl.find_opt store key with
          | Some _ -> Hashtbl.replace store key value
          | None -> Hashtbl.add store key value)
        s
    in
    let update_tmp tid (k, v) =
      match Hashtbl.find_opt tmp tid with
      | Some s ->
          let s' =
            StrMap.update k (function Some _ -> Some v | None -> Some v) s
          in
          Hashtbl.replace tmp tid s'
      | None -> Hashtbl.add tmp tid StrMap.empty
    in
    let find_tmp tid k =
      match Hashtbl.find_opt tmp tid with
      | Some s -> StrMap.find_opt s k
      | None -> None
    in
    List.fold_left
      (fun res log ->
        if res then
          match log with
          | Begin { tid } ->
              Hashtbl.add tmp tid StrMap.empty;
              res
          | Commit { tid; _ } -> (
              match Hashtbl.find_opt tmp tid with
              | Some s ->
                  apply_s s;
                  Hashtbl.remove tmp tid;
                  res
              | None -> false)
          | Put { tid; key; value } ->
              update_tmp tid (key, value);
              res
          | Get { tid; key; value; _ } -> (
              match find_tmp tid key with
              | Some value' ->
                  if Yojson.Basic.equal value value' then res
                  else
                    let () =
                      Pp.printf "get %s -> %s | %s\n" key
                        (Yojson.Basic.to_string value)
                        (Yojson.Basic.to_string value')
                    in
                    false
              | None -> (
                  match Hashtbl.find_opt store key with
                  | Some value' ->
                      if Yojson.Basic.equal value value' then res
                      else
                        let () =
                          Pp.printf "get %s -> %s | %s\n" key
                            (Yojson.Basic.to_string value)
                            (Yojson.Basic.to_string value')
                        in
                        false
                  | None -> false))
        else false)
      true !_history

  (* +----------------+------------------------+
     | tableName:key  | {tid: int, json: json} |
     +----------------+------------------------+
     |  cart:3        | {tid: 1, json: [2]}    |
     +----------------+------------------------+ *)

  (* let get_cur_tid thread_id =
    match Hashtbl.find_opt so thread_id with
    | Some l -> (
        match List.rev l with
        | [] -> Zutils.(_die_with [%here] "thread_id not found")
        | x :: _ -> x)
    | None -> Zutils.(_die_with [%here] "thread_id not found") *)

  (* let check_validate thread_id tid =
    let tid' = get_cur_tid thread_id in
    if tid' != tid then Zutils.(_die_with [%here] "tid is not valid") else () *)

  let get_conn tid =
    match Hashtbl.find_opt connMap tid with
    | Some conn -> Hashtbl.find connectionPool conn
    | None -> Zutils.(_die_with [%here] "tid not found")

  let gelara1_port = 3307
  let gelara2_port = 3308
  let gelara3_port = 3309

  let async_clear_db () =
    let () = Hashtbl.clear so in
    let () = wr := [] in
    let () = co := [] in
    let* conn = connect gelara1_port "mysql" >>= or_die "connect" in
    let* _ =
      no_param_no_ret conn
        (Printf.sprintf "DROP DATABASE IF EXISTS %s;" !_db_name)
    in
    let* _ =
      no_param_no_ret conn (Printf.sprintf "CREATE DATABASE %s;" !_db_name)
    in
    let* _ = no_param_no_ret conn (Printf.sprintf "USE %s;" !_db_name) in
    let* _ =
      no_param_no_ret conn (Printf.sprintf "DROP TABLE IF EXISTS %s" !_db_name)
    in
    let* _ =
      no_param_no_ret conn
        (Printf.sprintf
           "CREATE TABLE %s (\n\
           \  id VARCHAR(255) PRIMARY KEY,\n\
           \  %s JSON NOT NULL\n\
            );"
           !_db_name feild_name)
    in
    let _ = M.close conn in
    let* _ =
      Hashtbl.fold
        (fun _ conn _ -> no_param_no_ret conn "ROLLBACK")
        connectionPool Lwt.return_unit
    in
    let () =
      Hashtbl.filter_map_inplace (fun _ _ -> Some true) connectionStatus
    in
    Lwt.return_unit

  let async_init_db () =
    let* () = async_clear_db () in
    let mk_conn port =
      let* conn = connect port !_db_name >>= or_die "connect" in
      let () = Hashtbl.add connectionPool port conn in
      let () = Hashtbl.add connectionStatus port true in
      let* _ = M.autocommit conn false >>= or_die "autocommit" in
      let () =
        Printf.printf "[DB] set isolation level %s\n"
          (Language.show_isolation !_isolation)
      in
      let* _ =
        match !_isolation with
        | Serializable ->
            no_param_no_ret conn
              "SET GLOBAL TRANSACTION ISOLATION LEVEL SERIALIZABLE;"
        | ReadUncommitted ->
            no_param_no_ret conn
              "SET GLOBAL TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;"
        | ReadCommitted ->
            let* _ =
              no_param_no_ret conn
                "SET GLOBAL TRANSACTION ISOLATION LEVEL READ COMMITTED;"
            in
            no_param_no_ret conn "SET GLOBAL wsrep_sync_wait = 0;"
        | Causal ->
            let* _ =
              no_param_no_ret conn
                "SET GLOBAL TRANSACTION ISOLATION LEVEL READ COMMITTED;"
            in
            no_param_no_ret conn "SET GLOBAL wsrep_sync_wait = 1;"
      in
      Lwt.return_unit
    in
    let* () = mk_conn gelara1_port in
    let* () = mk_conn gelara2_port in
    let* () = mk_conn gelara3_port in
    Lwt.return_unit

  let init db_name isolation =
    _db_name := db_name;
    _isolation := isolation;
    Lwt_main.run (async_init_db ())

  let raw_clear_db () = Lwt_main.run (async_clear_db ())

  let close () =
    let r =
      let* _ =
        Hashtbl.fold
          (fun _ conn _ ->
            let* _ = M.close conn in
            Lwt.return_unit)
          connectionPool Lwt.return_unit
      in
      M.library_end ();
      Lwt.return_unit
    in
    Lwt_main.run r

  let async_begin ~thread_id () =
    let tid = next_tid () in
    let connId = use_connection () in
    Hashtbl.add connMap tid connId;
    let conn = Hashtbl.find connectionPool connId in
    let* _ = no_param_no_ret conn "BEGIN" in
    let () = Printf.printf "[DB] begin tid: %i with port %i\n" tid connId in
    let () = _history := !_history @ [ Begin { tid } ] in
    let () =
      match Hashtbl.find_opt so thread_id with
      | Some l -> Hashtbl.replace so thread_id (l @ [ tid ])
      | None -> Hashtbl.add so thread_id [ tid ]
    in
    Lwt.return tid

  let raw_begin ~thread_id = Lwt_main.run (async_begin ~thread_id ())

  let async_commit ~tid () =
    (* let () = check_validate thread_id tid in *)
    match Hashtbl.find_opt commit_tid tid with
    | Some cid ->
        Zutils.(
          _die_with [%here]
            (spf "transaction %i already committed with cid %i" tid cid))
    | None ->
        let cid = next_cid () in
        let () = Printf.printf "[DB] commit {tid: %i, cid: %i}\n" tid cid in
        let () = Hashtbl.add commit_tid tid cid in
        let () =
          match Hashtbl.find_opt connMap tid with
          | Some connId -> Hashtbl.replace connectionStatus connId true
          | None -> Zutils.(_die_with [%here] "tid not found")
        in
        let conn = get_conn tid in
        let* _ = no_param_no_ret conn "COMMIT" in
        let () = co := !co @ [ tid ] in
        let () = _history := !_history @ [ Commit { tid; cid } ] in
        Lwt.return cid

  let raw_commit ~tid = Lwt_main.run (async_commit ~tid ())

  let async_release_connection ~tid () =
    (* let () = check_validate thread_id tid in *)
    match Hashtbl.find_opt commit_tid tid with
    | Some _ -> Lwt.return_unit
    | None ->
        let connId =
          match Hashtbl.find_opt connMap tid with
          | Some connId -> connId
          | None -> Zutils.(_die_with [%here] "tid not found")
        in
        let conn = get_conn tid in
        let* _ = no_param_no_ret conn "ROLLBACK" in
        let () = Hashtbl.replace connectionStatus connId true in
        let () = Hashtbl.remove connMap tid in
        Lwt.return_unit

  let async_get_current_isolation ~tid () =
    let conn = get_conn tid in
    let* stmt =
      M.prepare conn (Zutils.spf "SELECT @@transaction_isolation;")
      >>= or_die "prepare"
    in
    let* res = M.Stmt.execute stmt [||] >>= or_die "exec" in
    let* s = res_to_list res in
    let () =
      Printf.printf "[DB] transaction_isolation (%i):\n" (List.length s)
    in
    let* () = print_row (List.nth s 0) in
    let* _ = M.Stmt.close stmt >>= or_die "stmt close" in
    Lwt.return_unit

  let raw_get_current_isolation ~tid =
    Lwt_main.run (async_get_current_isolation ~tid ())

  let async_put ~tid ~table ~key ~json () =
    (* let () = check_validate thread_id tid in *)
    let conn = get_conn tid in
    let json =
      `Assoc
        [ (wrapper_tid_json_feild, `Int tid); (wrapper_value_json_feild, json) ]
    in
    let json_str = Yojson.Basic.to_string json in
    let raw_key = Zutils.spf "%s:%s" table key in
    let () =
      Printf.printf "[DB] put {tid: %i, key: %s, value: %s}\n" tid raw_key
        json_str
    in
    (* let* () = async_get_current_isolation conn () in *)
    (* let () = Language.(if !__counter == 10 then _die_with [%here] "die") in *)
    let* stmt =
      M.prepare conn
        (Zutils.spf
           "INSERT INTO %s (id, %s) VALUES (?, ?) ON DUPLICATE KEY UPDATE %s = \
            ?"
           table feild_name feild_name)
      >>= or_die "prepare"
    in
    let* _ =
      M.Stmt.execute stmt
        [| `String raw_key; `String json_str; `String json_str |]
      >>= or_die "exec"
    in
    let () = Language.(if !__counter == 10 then _die_with [%here] "die") in
    let* _ = M.Stmt.close stmt >>= or_die "stmt close" in
    let () = _history := !_history @ [ Put { tid; key; value = json } ] in
    Lwt.return_unit

  let raw_put ~tid ~table ~key ~json =
    Lwt_main.run (async_put ~tid ~table ~key ~json ())

  let layout_field field =
    match M.Field.value field with
    | `Int i -> Lwt_io.printf "%d\n%!" i
    | `Float x -> Lwt_io.printf "%f\n%!" x
    | `String s -> Lwt_io.printf "%s\n%!" s
    | `Bytes b -> Lwt_io.printf "%s\n%!" (Bytes.to_string b)
    | `Time t ->
        Lwt_io.printf "%04d-%02d-%02d %02d:%02d:%02d\n%!" (M.Time.year t)
          (M.Time.month t) (M.Time.day t) (M.Time.hour t) (M.Time.minute t)
          (M.Time.second t)
    | `Null -> Lwt_io.printf "NULL\n%!"

  let async_get ~tid ~table ~key () =
    (* let () = check_validate thread_id tid in *)
    let conn = get_conn tid in
    let raw_key = Zutils.spf "%s:%s" table key in
    let* stmt =
      M.prepare conn
        (Zutils.spf "SELECT values_json FROM %s WHERE id = ?" table)
      >>= or_die "prepare"
    in
    let* res = M.Stmt.execute stmt [| `String raw_key |] >>= or_die "exec" in
    let* s = res_to_list res in
    let* json =
      match s with
      | [] ->
          let* _ = M.Stmt.close stmt >>= or_die "stmt close" in
          Zutils.(raise (DBKeyNotFound (spf "key %s not found" raw_key)))
      | [ row ] ->
          let json = M.Row.StringMap.find feild_name row in
          let json =
            match M.Field.value json with
            | `String s -> Yojson.Basic.from_string s
            | `Null -> Zutils.(_die_with [%here] "invalid null")
            | `Int _ -> Zutils.(_die_with [%here] "invalid int")
            | `Float _ -> Zutils.(_die_with [%here] "invalid float")
            | `Bytes bytes -> Yojson.Basic.from_string (Bytes.to_string bytes)
            | `Time _ -> Zutils.(_die_with [%here] "invalid time")
          in
          let () =
            Printf.printf "[DB] get {tid: %i, key: %s, value: %s}\n" tid raw_key
              (Yojson.Basic.to_string json)
          in
          Lwt.return json
      | _ ->
          Zutils.(
            _die_with [%here] (spf "key %s store invalid valures" raw_key))
    in
    let* _ = M.Stmt.close stmt >>= or_die "stmt close" in
    let prev_tid =
      json
      |> Yojson.Basic.Util.member wrapper_tid_json_feild
      |> Yojson.Basic.Util.to_int
    in
    let prev_cid =
      match Hashtbl.find_opt commit_tid prev_tid with
      | Some cid -> cid
      | None -> (
          (* let () =
            Hashtbl.iter
              (fun tid cid ->
                Printf.printf "<committed> tid: %i with cid %i\n" tid cid)
              commit_tid
          in *)
          match !_isolation with
          | ReadUncommitted -> -99
          | _ ->
              Zutils.(
                _die_with [%here] (spf "tid %i is not committed yet" prev_tid)))
    in
    let () = _history := !_history @ [ Get { tid; key; value = json } ] in
    let json = json |> Yojson.Basic.Util.member wrapper_value_json_feild in
    let () = wr := !wr @ [ (prev_tid, tid) ] in
    Lwt.return (prev_tid, prev_cid, json)

  let raw_get ~tid ~table ~key = Lwt_main.run (async_get ~tid ~table ~key ())

  let maria_context db_name isolation k =
    let () = init db_name isolation in
    let () = k () in
    let () = close () in
    ()

  let table_async_get ~tid ~db ~table ~key () =
    async_get ~tid ~table:db ~key:(table ^ ":" ^ key) ()

  let table_async_put ~tid ~db ~table ~key ~json () =
    async_put ~tid ~table:db ~key:(table ^ ":" ^ key) ~json ()
end

open MyMariaDB

let run_in_parallel f =
  let f () = Lwt_main.run (f ()) in
  Lwt_preemptive.detach f ()

let test_dirty_read_concurrent isolation () =
  let isolation = Language.isolation_of_string isolation in
  let db_name = "dirty_read_concurrent" in
  maria_context db_name isolation (fun () ->
      let table = "dirty_read_concurrent" in
      let key = "3" in
      let thread1 n =
        let thread_id = 1 in
        let* tid = async_begin ~thread_id () in
        let* _ = async_put ~tid ~table ~key ~json:(`Int n) () in
        let* _ = Lwt_unix.sleep (Random.float 0.01) in
        async_commit ~tid ()
      in
      let thread3 _ =
        let thread_id = 3 in
        let* tid = async_begin ~thread_id () in
        let* _, _, _ = async_get ~tid ~table ~key () in
        let* _ = Lwt_unix.sleep (Random.float 0.01) in
        let* _, _, _ = async_get ~tid ~table ~key () in
        let* _ = async_commit ~tid () in
        Lwt.return_unit
        (* if Yojson.Basic.equal res1 res2 then Lwt.return_unit
    else
      Lwt.fail_with
        (Printf.sprintf "%s != %s"
           (Yojson.Basic.to_string res1)
           (Yojson.Basic.to_string res2)) *)
      in
      let _ = Lwt_main.run (thread1 1) in
      let rec mk_loop n f () =
        if n <= 0 then Lwt.return_unit
        else
          let* _ = f n in
          let* _ = Lwt_unix.sleep (Random.float 0.01) in
          mk_loop (n - 1) f ()
      in
      let test m =
        Lwt_main.run (Lwt.join [ mk_loop m thread1 (); mk_loop m thread3 () ])
      in
      let () = test 10 in
      let () =
        Printf.printf "check_read_committed: %b\n" (check_read_committed ())
      in
      let () =
        Printf.printf "check_serializable: %b\n" (check_serializable ())
      in
      let () = Printf.printf "check_causal: %b\n" (check_causal ()) in
      ())

let test_stuck isolation () =
  let isolation = Language.isolation_of_string isolation in
  let db_name = "stuck" in
  maria_context db_name isolation (fun () ->
      let table = "stuck" in
      let tid1 = raw_begin ~thread_id:1 in
      let tid2 = raw_begin ~thread_id:2 in
      let () = raw_put ~tid:tid1 ~table ~key:"3" ~json:(`List [ `Int 1 ]) in
      let () = raw_put ~tid:tid2 ~table ~key:"3" ~json:(`List [ `Int 2 ]) in
      let _ = raw_commit ~tid:tid1 in
      let _ = raw_commit ~tid:tid2 in
      ())

let test_dirty_read isolation () =
  let isolation = Language.isolation_of_string isolation in
  let db_name = "dirty_read" in
  maria_context db_name isolation (fun () ->
      let table = "dirty_read" in
      let tid1 = raw_begin ~thread_id:1 in
      let _ = Printf.printf "tid1: %i\n" tid1 in
      let () = raw_put ~tid:tid1 ~table ~key:"3" ~json:(`List [ `Int 2 ]) in
      let tid2 = raw_begin ~thread_id:2 in
      let prev_tid, prev_cid, result = raw_get ~tid:tid2 ~table ~key:"3" in
      let _ =
        Printf.printf "read 1 result: %s\n" (Yojson.Basic.to_string result)
      in
      let _ = Printf.printf "prev_tid: %i\n" prev_tid in
      let _ = Printf.printf "prev_cid: %i\n" prev_cid in
      let _ = raw_commit ~tid:tid1 in
      let _ = raw_commit ~tid:tid2 in
      ())

let test_non_repeatable_read isolation () =
  let isolation = Language.isolation_of_string isolation in
  let db_name = "non_repeatable_read" in
  maria_context db_name isolation (fun () ->
      let table = "non_repeatable_read" in
      let tid1 = raw_begin ~thread_id:1 in
      let _ = Printf.printf "tid1: %i\n" tid1 in
      let () = raw_put ~tid:tid1 ~table ~key:"3" ~json:(`List [ `Int 2 ]) in
      let _ = raw_commit ~tid:tid1 in
      let tid2 = raw_begin ~thread_id:2 in
      let tid3 = raw_begin ~thread_id:3 in
      let prev_tid, prev_cid, result = raw_get ~tid:tid2 ~table ~key:"3" in
      let _ =
        Printf.printf "read 1 result: %s\n" (Yojson.Basic.to_string result)
      in
      let _ = Printf.printf "prev_tid: %i\n" prev_tid in
      let _ = Printf.printf "prev_cid: %i\n" prev_cid in
      let () =
        raw_put ~tid:tid3 ~table ~key:"3" ~json:(`List [ `Int 3; `Int 4 ])
      in
      let _ = raw_commit ~tid:tid3 in
      let prev_tid, prev_cid, result = raw_get ~tid:tid2 ~table ~key:"3" in
      let _ =
        Printf.printf "read 2 result: %s\n" (Yojson.Basic.to_string result)
      in
      let _ = Printf.printf "prev_tid: %i\n" prev_tid in
      let _ = Printf.printf "prev_cid: %i\n" prev_cid in
      let _ = raw_commit ~tid:tid2 in
      ())

let test_causal isolation () =
  let isolation = Language.isolation_of_string isolation in
  let db_name = "causal" in
  let table = "causal" in
  maria_context db_name isolation (fun () ->
      let tid1 = raw_begin ~thread_id:1 in
      let _ = Printf.printf "tid1: %i\n" tid1 in
      let () = raw_put ~tid:tid1 ~table ~key:"1" ~json:(`Int 1) in
      let () = raw_put ~tid:tid1 ~table ~key:"2" ~json:(`Int 1) in
      let _ = raw_commit ~tid:tid1 in
      let tid2 = raw_begin ~thread_id:2 in
      let prev_tid, prev_cid, result = raw_get ~tid:tid2 ~table ~key:"1" in
      let _ =
        Printf.printf "read 1 result: %s\n" (Yojson.Basic.to_string result)
      in
      let _ = Printf.printf "prev_tid: %i\n" prev_tid in
      let _ = Printf.printf "prev_cid: %i\n" prev_cid in
      let tid3 = raw_begin ~thread_id:3 in
      let _ = raw_get ~tid:tid3 ~table ~key:"1" in
      let () = raw_put ~tid:tid3 ~table ~key:"1" ~json:(`Int 2) in
      let () = raw_put ~tid:tid3 ~table ~key:"2" ~json:(`Int 2) in
      let _ = raw_commit ~tid:tid3 in
      let prev_tid, prev_cid, result = raw_get ~tid:tid2 ~table ~key:"2" in
      let _ =
        Printf.printf "read 2 result: %s\n" (Yojson.Basic.to_string result)
      in
      let _ = Printf.printf "prev_tid: %i\n" prev_tid in
      let _ = Printf.printf "prev_cid: %i\n" prev_cid in
      let _ = raw_commit ~tid:tid2 in
      ())

let test_cart isolation () =
  let isolation = Language.isolation_of_string isolation in
  let db_name = "cart" in
  let table = "cart" in
  maria_context db_name isolation (fun () ->
      let tid1 = raw_begin ~thread_id:1 in
      let _ = Printf.printf "tid1: %i\n" tid1 in
      let () = raw_put ~tid:tid1 ~table ~key:"3" ~json:(`List [ `Int 2 ]) in
      let _ = raw_commit ~tid:tid1 in
      let tid2 = raw_begin ~thread_id:2 in
      let () =
        raw_put ~tid:tid2 ~table ~key:"3" ~json:(`List [ `Int 3; `Int 4 ])
      in
      let tid3 = raw_begin ~thread_id:3 in
      let prev_tid, prev_cid, result = raw_get ~tid:tid3 ~table ~key:"3" in
      let _ = raw_commit ~tid:tid2 in
      let _ = raw_commit ~tid:tid3 in
      let _ = Printf.printf "result: %s\n" (Yojson.Basic.to_string result) in
      let _ = Printf.printf "prev_tid: %i\n" prev_tid in
      let _ = Printf.printf "prev_cid: %i\n" prev_cid in
      ())
