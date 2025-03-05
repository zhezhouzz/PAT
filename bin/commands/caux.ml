open Core

let regular_file =
  Command.Arg_type.create (fun filename ->
      if Stdlib.Sys.is_regular_file filename then filename
      else failwith "Not a regular file")

(** [dir_is_empty dir] is true, if [dir] contains no files except * "." and ".."
*)
let dir_is_empty dir = Array.length (Stdlib.Sys.readdir dir) = 0

(** [dir_contents] returns the paths of all regular files that are * contained
    in [dir]. Each file is a path starting with [dir]. *)
let dir_contents dir =
  let rec loop result = function
    | f :: fs -> (
        match Stdlib.Sys.is_directory f with
        | true ->
            Stdlib.Sys.readdir f |> Array.to_list
            |> List.map ~f:(Filename.concat f)
            |> List.append fs |> loop result
        | _ -> loop (f :: result) fs)
    | [] -> result
  in
  loop [] [ dir ]
