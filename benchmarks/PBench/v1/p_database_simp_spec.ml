(* Simplified from p_database_spec.ml
   Original: Read-your-writes spec with detailed qualifiers linking write/read values.
   Simplified: Observation events (readRsp, writeRsp) use `true` - the value linking
   is preserved in readReq's two cases and the goal. Removed redundant payload
   constraints from observation uHATs. *)
val ( == ) : 'a. 'a -> 'a -> bool
val readReq : < > [@@gen]
val readRsp : < va : int ; st : bool > [@@obsRecv]
val writeReq : < va : int > [@@gen]
val writeRsp : < va : int > [@@obsRecv]

let readReq =
  [|
    (fun (x : int) ->
      ( (allA;
         WriteReq (va == x);
         starA (anyA - WriteReq true)),
        ReadReq true,
        [| ReadRsp (va == x && st) |] ));
    (starA (anyA - WriteReq true), ReadReq true, [| ReadRsp (not st) |]);
  |]

let writeReq ?l:(x = (true : [%v: int])) =
  (allA, WriteReq (va == x), [| WriteRsp (va == x) |])

let writeRsp ?l:(x = (true : [%v: int])) = (allA, WriteRsp true, [||])

let readRsp ?l:(x = (true : [%v: int])) ?l:(s = (true : [%v: bool])) =
  (allA, ReadRsp true, [||])

(* read your write - UNCHANGED *)
let[@goal] p_database (x : int) (y : int) =
  allA;
  WriteRsp (va == x);
  starA (anyA - WriteRsp true);
  ReadRsp (va == y && (not (x == y)) && st);
  allA
