open Language
(* open Zdatatype *)

type ev = { op : string; args : value list } [@@deriving show, eq, ord]
type msg = { src : int; dest : int option; ev : ev } [@@deriving show, eq, ord]
type handler = { tid : int; op : string; k : msg -> unit }
type async_handler = { has_ret : bool; k : ev -> ev }
type controller = { tid : int; code : term; store : Store.t }

exception RuntimeInconsistent of string

type _ Effect.t +=
  | Send : msg -> unit Effect.t
  | Recv : string -> msg Effect.t
  | Gen : msg -> unit Effect.t
  | Obs : (string * (ev -> bool)) -> msg option Effect.t
  | Async : msg -> msg Effect.t
  | End : unit Effect.t

let layout_ev { op; args } =
  Printf.sprintf "%s(%s)" op (String.concat ", " (List.map layout_value args))

let layout_msg { src; dest; ev } =
  Printf.sprintf "[%s](%i -> %s)\n" (layout_ev ev) src
    (match dest with None -> "*" | Some dest -> string_of_int dest)

let layout_msg_concise { ev; _ } = Printf.sprintf "[%s] " (layout_ev ev)

type config = (string * int) list

let get_config_value_opt config key =
  List.find_opt (fun (k, _) -> String.equal k key) config |> Option.map snd

let get_config_value config key =
  match get_config_value_opt config key with
  | Some value -> value
  | None -> _die_with [%here] (Printf.sprintf "config key %s not found" key)

type database_ctx = { dbname : string; isolation : isolation }

type ocaml_test_env = {
  if_concurrent : bool;
  database_ctx : database_ctx option;
  init_test_env : unit -> unit;
  default_test_prog : term list;
  property : msg list -> bool;
  random_test_gen : config -> unit;
}
