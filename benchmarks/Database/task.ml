val ( == ) : 'a -> 'a -> bool
val readReq : unit [@@gen]
val readRsp : < va : int ; st : bool > [@@obsRecv]
val writeReq : < va : int > [@@gen]
val writeRsp : < va : int ; st : bool > [@@obsRecv]

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

let writeRsp ?l:(x = (true : [%v: int])) ?l:(s = (true : [%v: bool])) =
  (allA, WriteRsp (va == x && st == s), [||])

let readRsp ?l:(x = (true : [%v: int])) ?l:(s = (true : [%v: bool])) =
  (allA, ReadRsp (va == x && st == s), [||])

let[@goal] read_your_write (x : int) (y : int) =
  not
    (allA;
     WriteRsp (va == x && st);
     starA (anyA - WriteRsp st);
     ReadRsp (va == y && (not (x == y)) && st);
     allA)
