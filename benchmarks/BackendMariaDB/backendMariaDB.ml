open Lwt.Infix
open Printf
module S = Mariadb.Nonblocking.Status

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

let connect db_name =
  M.connect
    ~host:(env "OCAML_MARIADB_HOST" "localhost")
    ~user:(env "OCAML_MARIADB_USER" "test")
    ~pass:(env "OCAML_MARIADB_PASS" "maria9")
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

open Lwt.Syntax

let no_param_no_ret db query =
  let* stmt = M.prepare db query >>= or_die "prepare" in
  M.Stmt.execute stmt [||] >>= or_die "exec" >>= fun _ ->
  M.Stmt.close stmt >>= or_die "stmt close" >>= fun () -> Lwt.return_unit

module MyMariaDB = struct
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
  let connMap : (int, M.t) Hashtbl.t = Hashtbl.create 10
  let _db_name = ref "cart"
  let feild_name = "values_json"
  let wrapper_tid_json_feild = "tid"
  let wrapper_value_json_feild = "vv"

  (* +----------------+------------------------+
     | tableName:key  | {tid: int, json: json} |
     +----------------+------------------------+
     |  cart:3        | {tid: 1, json: [2]}    |
     +----------------+------------------------+ *)

  let get_conn thread_id k =
    match Hashtbl.find_opt connMap thread_id with
    | Some conn -> k conn
    | None ->
        let* conn = connect !_db_name >>= or_die "connect" in
        let* _ = no_param_no_ret conn (Printf.sprintf "USE %s;" !_db_name) in
        Hashtbl.add connMap thread_id conn;
        k conn

  let init_cart () =
    let* conn =
      M.connect
        ~host:(env "OCAML_MARIADB_HOST" "localhost")
        ~user:(env "OCAML_MARIADB_USER" "test")
        ~pass:(env "OCAML_MARIADB_PASS" "maria9")
        ()
      >>= or_die "connect"
    in
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
    Lwt.return_unit

  let init db_name =
    _db_name := db_name;
    Lwt_main.run (init_cart ())

  let close () =
    let r =
      let* _ =
        Hashtbl.fold
          (fun _ conn _ ->
            let* _ = M.close conn in
            Lwt.return_unit)
          connMap Lwt.return_unit
      in
      M.library_end ();
      Lwt.return_unit
    in
    Lwt_main.run r

  let raw_begin ~thread_id =
    let _ =
      Lwt_main.run
        (get_conn thread_id (fun conn -> no_param_no_ret conn "BEGIN"))
    in
    let tid = next_tid () in
    tid

  let raw_commit ~thread_id ~tid =
    match Hashtbl.find_opt commit_tid tid with
    | Some cid ->
        Zutils.(
          _die_with [%here]
            (spf "transaction %i already committed with cid %i" tid cid))
    | None ->
        let cid = next_cid () in
        let () = Printf.printf "commit tid: %i with cid %i\n" tid cid in
        let () = Hashtbl.add commit_tid tid cid in
        let _ =
          Lwt_main.run
            (get_conn thread_id (fun conn -> no_param_no_ret conn "COMMIT"))
        in
        cid

  let raw_put ~thread_id ~tid ~table ~key ~json =
    let raw_key = Zutils.spf "%s:%s" table key in
    let raw_json =
      Yojson.Basic.to_string
        (`Assoc
           [
             (wrapper_tid_json_feild, `Int tid); (wrapper_value_json_feild, json);
           ])
    in
    let r =
      get_conn thread_id (fun conn ->
          let* stmt =
            M.prepare conn
              (Zutils.spf
                 "INSERT INTO cart (id, %s) VALUES (?, ?) ON DUPLICATE KEY \
                  UPDATE %s = ?"
                 feild_name feild_name)
            >>= or_die "prepare"
          in
          let* _ =
            M.Stmt.execute stmt
              [| `String raw_key; `String raw_json; `String raw_json |]
            >>= or_die "exec"
          in
          Lwt.return_unit)
    in
    Lwt_main.run r

  let res_to_list res =
    let next _ =
      M.Res.fetch (module M.Row.Map) res >>= function
      | Ok (Some _ as row) -> Lwt.return row
      | Ok None -> Lwt.return_none
      | Error _ -> Lwt.return_none
    in
    Lwt_stream.to_list @@ Lwt_stream.from next

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

  let raw_get ~thread_id ~table ~key =
    let raw_key = Zutils.spf "%s:%s" table key in
    let r =
      get_conn thread_id (fun conn ->
          let* stmt =
            M.prepare conn
              (Zutils.spf "SELECT values_json FROM cart WHERE id = ?")
            >>= or_die "prepare"
          in
          let* res =
            M.Stmt.execute stmt [| `String raw_key |] >>= or_die "exec"
          in
          let* s = res_to_list res in
          match s with
          | [] -> Zutils.(_die_with [%here] (spf "key %s not found" raw_key))
          | [ row ] ->
              let json = M.Row.StringMap.find feild_name row in
              Lwt.return json
          | _ ->
              Zutils.(
                _die_with [%here] (spf "key %s store invalid valures" raw_key)))
    in
    let json = Lwt_main.run r in
    let json =
      match M.Field.value json with
      | `String s -> Yojson.Basic.from_string s
      | `Null -> Zutils.(_die_with [%here] "invalid null")
      | `Int _ -> Zutils.(_die_with [%here] "invalid int")
      | `Float _ -> Zutils.(_die_with [%here] "invalid float")
      | `Bytes bytes -> Yojson.Basic.from_string (Bytes.to_string bytes)
      | `Time _ -> Zutils.(_die_with [%here] "invalid time")
      (* let () = Lwt_main.run (layout_field json) in *)
      (* Zutils.(_die_with [%here] "invalid json") *)
    in
    let prev_tid =
      json
      |> Yojson.Basic.Util.member wrapper_tid_json_feild
      |> Yojson.Basic.Util.to_int
    in
    let prev_cid =
      match Hashtbl.find_opt commit_tid prev_tid with
      | Some cid -> cid
      | None ->
          let () =
            Hashtbl.iter
              (fun tid cid ->
                Printf.printf "<committed> tid: %i with cid %i\n" tid cid)
              commit_tid
          in
          Zutils.(
            _die_with [%here] (spf "tid %i is not committed yet" prev_tid))
    in
    let json = json |> Yojson.Basic.Util.member wrapper_value_json_feild in
    (prev_tid, prev_cid, json)
end

open MyMariaDB

let test_cart () =
  let db_name = "cart" in
  let table = "cart" in
  let () = init db_name in
  let tid1 = raw_begin ~thread_id:1 in
  let _ = Printf.printf "tid1: %i\n" tid1 in
  let () =
    raw_put ~thread_id:1 ~tid:tid1 ~table ~key:"3" ~json:(`List [ `Int 2 ])
  in
  let _ = raw_commit ~thread_id:1 ~tid:tid1 in
  let tid2 = raw_begin ~thread_id:2 in
  let () =
    raw_put ~thread_id:2 ~tid:tid2 ~table ~key:"3" ~json:(`List [ `Int 3 ])
  in
  let tid3 = raw_begin ~thread_id:3 in
  let prev_tid, prev_cid, result = raw_get ~thread_id:3 ~table ~key:"3" in
  let _ = raw_commit ~thread_id:2 ~tid:tid2 in
  let _ = raw_commit ~thread_id:3 ~tid:tid3 in
  let _ = Printf.printf "result: %s\n" (Yojson.Basic.to_string result) in
  let _ = Printf.printf "prev_tid: %i\n" prev_tid in
  let _ = Printf.printf "prev_cid: %i\n" prev_cid in
  let () = close () in
  ()
