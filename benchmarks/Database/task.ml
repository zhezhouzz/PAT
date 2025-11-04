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

let writeRsp ?l:(x = (true : [%v: int])) = (allA, WriteRsp (va == x), [||])

let readRsp ?l:(x = (true : [%v: int])) ?l:(s = (true : [%v: bool])) =
  (allA, ReadRsp (va == x && st == s), [||])

(* read your write *)
let[@goal] task_Database (x : int) (y : int) =
  allA;
  WriteRsp (va == x);
  starA (anyA - WriteRsp true);
  ReadRsp (va == y && (not (x == y)) && st);
  allA
