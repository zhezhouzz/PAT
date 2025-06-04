open Sexplib.Std

type p_machine_instance = { mid : int; mname : string }
[@@deriving yojson, sexp, show, eq, ord]

type jevent = {
  log : string;
  event : string;
  state : string;
  sender : p_machine_instance;
  target : p_machine_instance;
  isTargetHalted : bool;
}
[@@deriving yojson, sexp, show, eq, ord]
