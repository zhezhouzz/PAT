val ( == ) : 'a -> 'a -> bool
val writeReq : < va : int > [@@gen]
val writeToMid : < va : int > [@@obs]
val writeToTail : < va : int > [@@obs]
val writeRsp : < va : int > [@@obsRecv]
val readReq : < va : int > [@@gen]
val readRsp : < va : int ; st : bool > [@@obsRecv]
val crashTail : unit [@@gen]

let writeReq ?l:(x = (true : [%v: int])) =
  (allA, WriteReq (va == x), [| WriteToMid (va == x) |])

let writeToMid ?l:(x = (true : [%v: int])) =
  (allA, WriteToMid (va == x), [| WriteToTail (va == x) |])

let writeToTail =
  [|
    (fun ?l:(x = (true : [%v: int])) ->
      ( starA (anyA - CrashTail true),
        WriteToTail (va == x),
        [| WriteRsp (va == x) |] ));
    (fun ?l:(x = (true : [%v: int])) ->
      ( (starA (anyA - CrashTail true);
         CrashTail true;
         starA (anyA - CrashTail true)),
        WriteToTail (va == x),
        [||] ));
  |]

let writeRsp ?l:(x = (true : [%v: int])) = (allA, WriteRsp (va == x), [||])

let readReq =
  [|
    (fun (x : int) ->
      ( (starA (anyA - CrashTail true);
         WriteToTail (va == x);
         starA (anyA - CrashTail true - WriteToTail true)),
        ReadReq true,
        [| ReadRsp (va == x && st) |] ));
    ( starA (anyA - CrashTail true - WriteToTail true),
      ReadReq true,
      [| ReadRsp (not st) |] );
    (fun (x : int) ->
      ( (starA (anyA - CrashTail true);
         CrashTail true;
         starA (anyA - CrashTail true);
         WriteToMid (va == x);
         starA (anyA - CrashTail true - WriteToMid true)),
        ReadReq true,
        [| ReadRsp (va == x && st) |] ));
    (fun (x : int) ->
      ( (starA (anyA - CrashTail true);
         WriteToMid (va == x);
         starA (anyA - CrashTail true - WriteToMid true);
         CrashTail true;
         starA (anyA - CrashTail true - WriteToMid true)),
        ReadReq true,
        [| ReadRsp (va == x && st) |] ));
    (fun (x : int) ->
      ( (starA (anyA - CrashTail true - WriteToMid true);
         CrashTail true;
         starA (anyA - CrashTail true - WriteToMid true)),
        ReadReq true,
        [| ReadRsp (not st) |] ));
  |]

let crashTail = (allA, CrashTail true, [||])

let[@goal] missingWriteRsp (x : int) =
  not
    (allA;
     WriteReq (va == x);
     starA (anyA - WriteRsp (va == x)))
