open Common
open Zdatatype
open Language

type buffer = msg list

let buffer = ref []
let init () = buffer := []
let add msg = let _ = Pp.printf "adding %s\n" msg.ev.op in buffer := msg :: !buffer

let find_by_op op =
  let rec aux = function
    | [] -> []
    | msg :: msgs -> let _ = Pp.printf "%s\n" msg.ev.op in
                     if msg.ev.op = op then msg :: aux msgs else aux msgs
  in
  aux !buffer

let is_empty () = !buffer = []
let layout () = List.split_by " | " layout_msg !buffer

let consume m =
  let () = Printf.printf "consume %s\n" (layout_msg m) in
  let rec aux rest = function
    | [] -> _die_with [%here] "No message found"
    | msg :: msgs ->
        if equal_msg msg m then rest @ msgs else aux (rest @ [ msg ]) msgs
  in
  let b = aux [] !buffer in
  buffer := b
