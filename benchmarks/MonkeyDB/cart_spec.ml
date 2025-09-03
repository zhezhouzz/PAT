val ( == ) : 'a. 'a -> 'a -> bool
(* Basic Typing *)

val begin : < tid : int > [@@obs]
val commit : < tid : int; cid: int > [@@obs]
val get : < tid : int; x : int; prevTid : int; prevCid : int; value: int list > [@@obs]
val put : < tid : int; x : int; ; value: int list > [@@obs]
val addItemReq : < user : int; item : int; > [@@gen]
val addItemResp : < > [@@obs]
val deleteItemReq : < user : int; item : int; > [@@gen]
val deleteItemResp : < > [@@obs]

(* PATs *)
let addItemReq ?l:(x = (true : [%v: int])) ?l:(y = (true : [%v: int])) =
  ( allA,
    AddItemReq (user == x && item == y),
    [| Begin true; Get true; Put true; Commit true; AddItemResp true |] )

let addItemResp = (allA, AddItemResp true, [||])

let deleteItemReq ?l:(x = (true : [%v: int])) ?l:(y = (true : [%v: int])) =
  ( allA,
    DeleteItemReq (user == x && item == y),
    [| Begin true; Get true; Put true; Commit true; DeleteItemResp true |] )

let deleteItemResp = (allA, DeleteItemResp true, [||])

(** TODO *)
let begin ?l:(x = (true : [%v: int])) = 
  (allA, Begin (tid == x), [| |])
let commit ?l:(x = (true : [%v: int])) ?l:(y = (true : [%v: int])) =
  (allA, Commit (tid == x && cid == y), [||])

let get ?l:(x = (true : [%v: int])) ?l:(y = (true : [%v: int]))
    ?l:(z = (true : [%v: int])) ?l:(w = (true : [%v: int])) =
  (allA, Get (tid == x && x == y && prevTid == z && prevCid == w), [||])

let put ?l:(x = (true : [%v: int])) ?l:(y = (true : [%v: int])) =
  (allA, Put (tid == x && x == y), [||])

(* Global Properties *)
let[@goal] serializable_violation (x : int) (y: int list) =
  allA;
  Put (key == x && value == y);
  starA (anyA - Put (key == x));
  Get (key == x && not (value == y));
  allA
