(* Simplified from p_simplified2pc_spec.ml
   Original: 2PC with put/commit/abort, read after write.
   Simplified: putRsp, writeRsp, readRsp observation qualifiers to true. writeReq
   refinement (v >= 0) simplified to (true : [%v: int]). putRsp two cases merged
   where possible - kept stat/not stat partition for control flow. *)
val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val readReq : < > [@@gen]
val getReq : < > [@@obs]
val readRsp : < va : int > [@@obsRecv]
val writeReq : < va : int > [@@gen]
val putReq : < va : int > [@@obs]
val putRsp : < va : int ; stat : bool > [@@obs]
val writeRsp : < va : int ; stat : bool > [@@obsRecv]
val commit : < > [@@obs]
val abort : < > [@@obs]

let readReq = (allA, ReadReq true, [| GetReq true |])
let getReq = (allA, GetReq true, [| ReadRsp (va == -1) |])
let readRsp ?l:(x = (true : [%v: int])) = (allA, ReadRsp true, [||])

let writeReq ?l:(x = (true : [%v: int])) =
  (allA, WriteReq (va == x), [| PutReq (va == x) |])

let putReq ?l:(x = (true : [%v: int])) =
  (allA, PutReq (va == x), [| PutRsp (va == x) |])

let putRsp =
 fun ?l:(x = (true : [%v: int])) ?l:(s = (v : [%v: bool])) ->
  (allA, PutRsp true, [| WriteRsp true; Commit true |])

let commit = (allA, Commit true, [||])
let abort = (allA, Abort true, [||])

let writeRsp ?l:(x = (true : [%v: int])) ?l:(s = (true : [%v: bool])) =
  (allA, WriteRsp true, [||])

(* read after write - UNCHANGED *)
let[@goal] p_simplified2pc (x : int) (y : int) =
  allA;
  WriteRsp (va == x && stat);
  starA (anyA - WriteRsp stat);
  ReadRsp (va == y && not (x == y));
  starA (anyA - ReadRsp true - WriteRsp true)
