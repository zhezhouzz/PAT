val ( == ) : 'a -> 'a -> bool
val readReq : < key : tKey > [@@gen]
val getReq : < key : tKey > [@@obs]
val readRsp : < key : tKey ; va : int > [@@obsRecv]
val writeReq : < key : tKey ; va : int > [@@gen]
val putReq : < key : tKey ; va : int > [@@obs]
val putRsp : < key : tKey ; va : int ; stat : bool > [@@obs]
val writeRsp : < key : tKey ; va : int ; stat : bool > [@@obsRecv]
val commit : < key : tKey > [@@obs]
val abort : < key : tKey > [@@obs]

let readReq ?l:(k = (true : [%v: tKey])) =
  (allA, ReadReq (key == k), [| GetReq (key == k) |])

let getReq ?l:(k = (true : [%v: tKey])) =
  ( starA (anyA - Commit (key == k)),
    GetReq (key == k),
    [| ReadRsp (key == k && va == -1) |] )

let readRsp ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int])) =
  (allA, ReadRsp (key == k && va == x), [||])

let writeReq ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int])) =
  (allA, WriteReq (key == k && va == x), [| PutReq (key == k && va == x) |])

let putReq ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int])) =
  (allA, PutReq (key == k && va == x), [| PutRsp (key == k && va == x) |])

let putRsp =
  [|
    (fun ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int]))
         ?l:(s = (v : [%v: bool])) ->
      ( allA,
        PutRsp (key == k && va == x && stat),
        [| WriteRsp (key == k && va == x && stat == s); Commit (key == k) |] ));
    (fun ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int]))
         ?l:(s = (not v : [%v: bool])) ->
      ( allA,
        PutRsp (key == k && va == x && stat == s),
        [| WriteRsp (key == k && va == x && not stat); Abort (key == k) |] ));
  |]

let commit ?l:(k = (true : [%v: tKey])) = (allA, Commit (key == k), [||])
let abort ?l:(k = (true : [%v: tKey])) = (allA, Abort (key == k), [||])

let writeRsp ?l:(x = (true : [%v: int])) ?l:(s = (v : [%v: bool])) =
  (allA, WriteRsp (va == x && stat == s), [||])

let[@goal] readAfterWrite (x : int) (y : int) =
  not
    (allA;
     WriteRsp (va == x && stat);
     starA (anyA - WriteRsp stat);
     ReadRsp (va == y && not (x == y));
     starA (anyA - ReadRsp true - WriteRsp true))
