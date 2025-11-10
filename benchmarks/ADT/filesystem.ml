open Language
open Interpreter
open Zdatatype

type fileKind = File | Directory

module CorrectFilesystem = struct
  type file = { kind : fileKind; name : string }
  type t = Node of file * t list

  let init fs = fs := Node ({ kind = Directory; name = "/" }, [])
  let divide_path_as_list path = String.split_on_char '/' path
  let new_file file_kind name = { kind = file_kind; name }
  let get_file = function Node (file, _) -> file
  let get_filenames l = List.map get_file l

  let filename_exists filename l =
    List.exists (fun file -> file.name = filename) l

  let exists_path fs path =
    if String.equal path "/" then true
    else
      let path = String.split_on_char '/' path in
      let rec aux tree path =
        match (tree, path) with
        | _, [] -> false
        | Node ({ name; _ }, _), [ filename ] -> String.equal name filename
        | Node ({ name; _ }, children), filename :: path ->
            if String.equal name filename then aux_multi children path
            else false
      and aux_multi trees path =
        match path with
        | [] -> _die [%here]
        | [ filename ] -> filename_exists filename (get_filenames trees)
        | filename :: path ->
            List.fold_left
              (fun res child ->
                if res then true
                else if String.equal (get_file child).name filename then
                  aux child path
                else false)
              false trees
      in
      aux !fs path

  let create_file fs file_kind path =
    let path = String.split_on_char '/' path in
    let ifSuccess = ref true in
    let rec aux tree path =
      match (tree, path) with
      | _, [] ->
          ifSuccess := false;
          tree
      | _, [ _ ] ->
          ifSuccess := false;
          tree
      | Node ({ name; kind = Directory }, children), filename :: path ->
          if String.equal name filename then
            Node ({ kind = Directory; name }, aux_multi children path)
          else (
            ifSuccess := false;
            tree)
      | Node ({ kind = File; _ }, _), _ :: _ ->
          ifSuccess := false;
          tree
    and aux_multi trees path =
      match path with
      | [] -> _die [%here]
      | [ filename ] ->
          if filename_exists filename (get_filenames trees) then (
            ifSuccess := false;
            trees)
          else Node (new_file file_kind filename, []) :: trees
      | filename :: path ->
          if not (filename_exists filename (get_filenames trees)) then (
            ifSuccess := false;
            trees)
          else
            List.map
              (fun child ->
                if String.equal (get_file child).name filename then
                  aux child path
                else child)
              trees
    in
    let new_fs = aux !fs path in
    if !ifSuccess then (
      fs := new_fs;
      true)
    else false

  let delete_file fs path =
    let path = String.split_on_char '/' path in
    let ifSuccess = ref true in
    let rec aux tree path =
      match (tree, path) with
      | _, [] ->
          ifSuccess := false;
          Some tree
      | Node ({ name; _ }, []), [ filename ] ->
          if String.equal name filename then None
          else (
            ifSuccess := false;
            Some tree)
      | Node ({ name; kind = Directory }, children), filename :: path ->
          if String.equal name filename then
            Some (Node ({ kind = Directory; name }, aux_multi children path))
          else (
            ifSuccess := false;
            Some tree)
      | Node ({ kind = File; _ }, _), _ :: _ ->
          ifSuccess := false;
          Some tree
    and aux_multi trees path = List.filter_map (fun t -> aux t path) trees in
    let new_fs = match aux !fs path with Some t -> t | None -> _die [%here] in
    if !ifSuccess then (
      fs := new_fs;
      true)
    else false

  let layout_filesystem fs =
    let rec aux depth tree =
      match tree with
      | Node ({ kind = Directory; name }, children) ->
          spf "%s%s\n%s" (String.make depth ' ') name
            (aux_multi (depth + 1) children)
      | Node ({ kind = File; name }, []) ->
          spf "%s%s" (String.make depth ' ') name
      | Node ({ kind = File; _ }, _) -> _die [%here]
    and aux_multi depth trees =
      List.split_by "\n" (fun t -> aux depth t) trees
    in
    aux !fs
end

module Filesystem = struct
  type file = FileNode of unit | DirNode of string list
  type t = file StrMap.t

  let init fs = fs := StrMap.add "/" (DirNode []) StrMap.empty
  let exists_path fs path = StrMap.mem path !fs

  let check_valid_path fs path =
    if not (StrMap.mem path !fs) then false
    else
      let rec aux path =
        if String.equal path "/" then true
        else
          match get_parent_path path with
          | None -> _die [%here]
          | Some "/" -> true
          | Some parent -> (
              match StrMap.find_opt !fs parent with
              | Some (DirNode _) -> aux parent
              | Some (FileNode _) -> false
              | None -> false)
      in
      aux path

  let is_dir fs path =
    match StrMap.find_opt !fs path with
    | Some (DirNode _) -> true
    | Some (FileNode _) -> false
    | None -> false

  let create_file fs file_kind path =
    match get_parent_path path with
    | None -> false
    | Some parent ->
        let () = Pp.printf "path: %s\n" path in
        let () = Pp.printf "parent: %s\n" parent in
        if check_valid_path fs parent && is_dir fs parent then (
          let file =
            match file_kind with File -> FileNode () | Directory -> DirNode []
          in
          fs := StrMap.add path file !fs;
          (* fs :=
            StrMap.update parent
              (fun x ->
                match x with
                | None -> _die [%here]
                | Some (DirNode children) -> Some (DirNode (path :: children))
                | Some (FileNode _) -> _die [%here])
              !fs; *)
          true)
        else false

  let correct_create_file fs file_kind path =
    match get_parent_path path with
    | None -> false
    | Some parent ->
        let () = Pp.printf "path: %s\n" path in
        let () = Pp.printf "parent: %s\n" parent in
        if check_valid_path fs parent && is_dir fs parent then (
          let file =
            match file_kind with File -> FileNode () | Directory -> DirNode []
          in
          fs := StrMap.add path file !fs;
          fs :=
            StrMap.update parent
              (fun x ->
                match x with
                | None -> _die [%here]
                | Some (DirNode children) -> Some (DirNode (path :: children))
                | Some (FileNode _) -> _die [%here])
              !fs;
          true)
        else false

  let delete_file fs path =
    if check_valid_path fs path then
      match StrMap.find_opt !fs path with
      | Some (DirNode []) | Some (FileNode _) ->
          fs := StrMap.remove path !fs;
          true
      | Some (DirNode _) -> false
      | None -> _die [%here]
    else false

  let correct_delete_file fs path =
    if check_valid_path fs path then
      match StrMap.find_opt !fs path with
      | Some (DirNode []) | Some (FileNode _) ->
          let parent =
            match get_parent_path path with None -> _die [%here] | Some x -> x
          in
          fs := StrMap.remove path !fs;
          fs :=
            StrMap.update parent
              (fun x ->
                match x with
                | None -> _die [%here]
                | Some (DirNode children) ->
                    Some (DirNode (List.filter (fun y -> y != path) children))
                | Some (FileNode _) -> _die [%here])
              !fs;
          true
      | Some (DirNode _) -> false
      | None -> _die [%here]
    else false

  let layout_filesystem fs =
    let rec aux depth (name, node) =
      match node with
      | DirNode children ->
          spf "%sDIR: %s\n%s" (String.make depth ' ') name
            (aux_multi (depth + 1) children)
      | FileNode () -> spf "%sFILE: %s" (String.make depth ' ') name
    and aux_multi depth nodes =
      let nodes =
        List.map (fun name -> (name, StrMap.find "die" !fs name)) nodes
      in
      List.split_by "\n" (fun (name, node) -> aux depth (name, node)) nodes
    in
    aux_multi 0 [ "/" ]
end

open Filesystem

let _fs : t ref = ref StrMap.empty

let initReqHandler (msg : msg) =
  let () = match msg.ev.args with [] -> () | _ -> _die [%here] in
  init _fs

let createReqHandler (msg : msg) =
  let path, isDir =
    match msg.ev.args with
    | [ VConst (S path); VConst (B isDir) ] -> (path, isDir)
    | _ -> _die [%here]
  in
  if isDir then
    let success = create_file _fs Directory path in
    send ("createResp", [ mk_value_bool success ])
  else
    let success = create_file _fs File path in
    send ("createResp", [ mk_value_bool success ])

let createFileReqHandler (msg : msg) =
  let path =
    match msg.ev.args with [ VConst (S path) ] -> path | _ -> _die [%here]
  in
  let success = create_file _fs File path in
  send ("createFileResp", [ mk_value_bool success ])

let createDirReqHandler (msg : msg) =
  let path =
    match msg.ev.args with [ VConst (S path) ] -> path | _ -> _die [%here]
  in
  let success = create_file _fs Directory path in
  send ("createDirResp", [ mk_value_bool success ])

let deleteReqHandler (msg : msg) =
  let path =
    match msg.ev.args with [ VConst (S path) ] -> path | _ -> _die [%here]
  in
  let success = delete_file _fs path in
  send ("deleteResp", [ mk_value_bool success ])

let deletePathReqHandler (msg : msg) =
  let path =
    match msg.ev.args with [ VConst (S path) ] -> path | _ -> _die [%here]
  in
  let success = delete_file _fs path in
  send ("deletePathResp", [ mk_value_bool success ])

let existsReqHandler (msg : msg) =
  let path =
    match msg.ev.args with [ VConst (S path) ] -> path | _ -> _die [%here]
  in
  let success = exists_path _fs path in
  send ("existsResp", [ mk_value_bool success ])

let existsPathReqHandler (msg : msg) =
  let path =
    match msg.ev.args with [ VConst (S path) ] -> path | _ -> _die [%here]
  in
  let success = exists_path _fs path in
  send ("existsPathResp", [ mk_value_bool success ])

let initRespHandler (_ : msg) = ()
let createFileRespHandler (_ : msg) = ()
let createDirRespHandler (_ : msg) = ()
let deletePathRespHandler (_ : msg) = ()
let existsPathRespHandler (_ : msg) = ()
let deleteRespHandler (_ : msg) = ()
let existsRespHandler (_ : msg) = ()
let createRespHandler (_ : msg) = ()

let init () =
  register_handler "initReq" initReqHandler;
  register_handler "createReq" createReqHandler;
  register_handler "existsReq" existsReqHandler;
  register_handler "deleteReq" deleteReqHandler;
  register_handler "createFileReq" createFileReqHandler;
  register_handler "createDirReq" createDirReqHandler;
  register_handler "deletePathReq" deletePathReqHandler;
  register_handler "existsPathReq" existsPathReqHandler;
  register_handler "deleteResp" deleteRespHandler;
  register_handler "existsResp" existsRespHandler;
  register_handler "createResp" createRespHandler;
  register_handler "createFileResp" createFileRespHandler;
  register_handler "createDirResp" createDirRespHandler;
  register_handler "deletePathResp" deletePathRespHandler;
  register_handler "existsPathResp" existsPathRespHandler;
  init _fs

open Nt

let record l = Ty_record { alias = None; fds = l }

let testCtx =
  Typectx.add_to_rights Typectx.emp
    [
      "initReq"#:(record []);
      "createFileReq"#:(record [ "path"#:string_ty ]);
      "createDirReq"#:(record [ "path"#:string_ty ]);
      "deletePathReq"#:(record [ "path"#:string_ty ]);
      "existsPathReq"#:(record [ "path"#:string_ty ]);
      "createFileResp"#:(record [ "success"#:bool_ty ]);
      "createDirResp"#:(record [ "success"#:bool_ty ]);
      "deletePathResp"#:(record [ "success"#:bool_ty ]);
      "existsPathResp"#:(record [ "success"#:bool_ty ]);
    ]

let gen name args body =
  mk_term_gen testCtx name (List.map (fun x -> VVar x) args) body

let obs name k = mk_term_obs_fresh testCtx name (fun _ -> k)
let obsInitResp e = mk_term_obs_fresh testCtx "initResp" (fun _ -> e)

let obsCreateFileResp e =
  mk_term_obs_fresh testCtx "createFileResp" (fun _ -> e)

let obsCreateDirResp e = mk_term_obs_fresh testCtx "createDirResp" (fun _ -> e)

let obsDeletePathResp e =
  mk_term_obs_fresh testCtx "deletePathResp" (fun _ -> e)

let obsExistsPathResp e =
  mk_term_obs_fresh testCtx "existsPathResp" (fun _ -> e)

let filesystem_last_delete trace =
  let fs = ref StrMap.empty in
  Filesystem.init fs;
  let rec check = function
    | [] -> true
    | { ev = { op = "initReq"; args = [] }; _ } :: rest ->
        Filesystem.init fs;
        check rest
    | {
        ev = { op = "createReq"; args = [ VConst (S path); VConst (B true) ] };
        _;
      }
      :: { ev = { op = "createResp"; args = [ VConst (B success) ] }; _ }
      :: rest ->
        let success' = Filesystem.correct_create_file fs Directory path in
        if success != success' then false else check rest
    | {
        ev = { op = "createReq"; args = [ VConst (S path); VConst (B false) ] };
        _;
      }
      :: { ev = { op = "createResp"; args = [ VConst (B success) ] }; _ }
      :: rest ->
        let success' = Filesystem.create_file fs File path in
        if success != success' then false else check rest
    | { ev = { op = "deleteReq"; args = [ VConst (S path) ] }; _ }
      :: { ev = { op = "deleteResp"; args = [ VConst (B success) ] }; _ }
      :: rest ->
        let success' = Filesystem.correct_delete_file fs path in
        if success != success' then false else check rest
    | _ :: rest -> check rest
  in
  check trace

let main =
  mk_term_assume_fresh
    (mk_p_abstract_ty "Path.t")
    (fun p1 ->
      let p1 = p1.x#:string_ty in
      lit_to_prop (mk_var_eq_c [%here] p1 (S "/a")))
    (fun p1 ->
      mk_term_assume_fresh
        (mk_p_abstract_ty "Path.t")
        (fun p2 ->
          let p2 = p2.x#:string_ty in
          lit_to_prop (mk_var_eq_c [%here] p2 (S "/a/b")))
        (fun p2 ->
          gen "createDirReq" [ p1 ] @@ obsCreateDirResp
          @@ gen "createDirReq" [ p2 ] @@ obsCreateDirResp
          @@ gen "deletePathReq" [ p2 ] @@ obsDeletePathResp
          @@ gen "deletePathReq" [ p1 ]
          @@ obsDeletePathResp mk_term_tt))

type filesystem_bench_config = { numOp : int }

let parse_config config =
  let numOp = get_config_value config "numOp" in
  { numOp }

let randomTest config =
  let { numOp } = parse_config config in
  let random_create_file () =
    let path = Sample.sample_by_ty (mk_p_abstract_ty "Path.t") in
    let b = Random.bool () in
    send ("createReq", [ path; mk_value_bool b ])
  in
  let random_delete_file () =
    let path = Sample.sample_by_ty (mk_p_abstract_ty "Path.t") in
    send ("deleteReq", [ path ])
  in
  let random_init () = send ("initReq", []) in
  let rec genOp restNum =
    if restNum <= 0 then ()
    else
      let () = Pp.printf "@{<yellow>restNum@}: %i\n" restNum in
      (match Random.int 3 with
      | 0 -> random_create_file ()
      | 1 -> random_delete_file ()
      | _ -> random_init ());
      genOp (restNum - 1)
  in
  let () = random_init () in
  let () = genOp numOp in
  let () = Pp.printf "@{<red>End with numOp@}\n%i\n" numOp in
  Effect.perform End

let test_env =
  {
    if_concurrent = false;
    database_ctx = None;
    init_test_env = init;
    default_test_prog = [ main ];
    property = filesystem_last_delete;
    random_test_gen = randomTest;
  }
