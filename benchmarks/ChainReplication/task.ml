val ( == ) : 'a -> 'a -> bool
val writeReq : < key : tKey ; va : int > [@@gen]

val writeToMid : < key : tKey ; va : int ; node : (node1 * node2[@tNode]) >
[@@obs]

val writeToTail : < key : tKey ; va : int > [@@obs]
val writeRsp : < key : tKey ; va : int > [@@obsRecv]
val readReq : < key : tKey > [@@gen]
val readRsp : < key : tKey ; va : int ; st : bool > [@@obsRecv]
val crashTail : unit [@@gen]

let writeReq ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int])) =
  ( allA,
    WriteReq (key == k && va == x),
    [|
      WriteToMid
        (key == k && va == x && node == ("Node1" : (node1 * node2[@tNode])));
    |] )

let writeToMid =
  [|
    (fun ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int]))
         ?l:(n =
             (v == ("Node1" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])])) ->
      ( allA,
        WriteToMid (key == k && va == x && node == n),
        [|
          WriteToMid
            (key == k && va == x && node == ("Node2" : (node1 * node2[@tNode])));
        |] ));
    (fun ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int]))
         ?l:(n =
             (v == ("Node2" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])])) ->
      ( allA,
        WriteToMid (key == k && va == x && node == n),
        [| WriteToTail (key == k && va == x) |] ));
  |]

let writeToTail =
  [|
    (fun ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int])) ->
      ( starA (anyA - CrashTail true),
        WriteToTail (key == k && va == x),
        [| WriteRsp (key == k && va == x) |] ));
    (fun ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int])) ->
      ( (starA (anyA - CrashTail true);
         CrashTail true;
         starA (anyA - CrashTail true)),
        WriteToTail (key == k && va == x),
        [||] ));
  |]

let writeRsp ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int])) =
  (allA, WriteRsp (key == k && va == x), [||])

let readReq =
  [|
    (fun (x : int) ?l:(k = (true : [%v: tKey])) ->
      ( (starA (anyA - CrashTail true);
         WriteToTail (key == k && va == x);
         starA (anyA - CrashTail true - WriteToTail true)),
        ReadReq (key == k),
        [| ReadRsp (key == k && va == x && st) |] ));
    (fun ?l:(k = (true : [%v: tKey])) ->
      ( starA (anyA - CrashTail true - WriteToTail (key == k)),
        ReadReq (key == k),
        [| ReadRsp (key == k && not st) |] ));
    (fun (x : int) ?l:(k = (true : [%v: tKey])) ->
      ( (starA (anyA - CrashTail true);
         CrashTail true;
         starA (anyA - CrashTail true);
         WriteToMid
           (key == k && va == x && node == ("Node2" : (node1 * node2[@tNode])));
         starA
           (anyA - CrashTail true
           - WriteToMid (key == k && node == ("Node2" : (node1 * node2[@tNode])))
           )),
        ReadReq (key == k),
        [| ReadRsp (key == k && va == x && st) |] ));
    (fun (x : int) ?l:(k = (true : [%v: tKey])) ->
      ( (starA (anyA - CrashTail true);
         WriteToMid
           (key == k && va == x && node == ("Node2" : (node1 * node2[@tNode])));
         starA
           (anyA - CrashTail true
           - WriteToMid (key == k && node == ("Node2" : (node1 * node2[@tNode])))
           );
         CrashTail true;
         starA
           (anyA - CrashTail true
           - WriteToMid (key == k && node == ("Node2" : (node1 * node2[@tNode])))
           )),
        ReadReq (key == k),
        [| ReadRsp (key == k && va == x && st) |] ));
    (fun (x : int) ?l:(k = (true : [%v: tKey])) ->
      ( (starA
           (anyA - CrashTail true
           - WriteToMid (key == k && node == ("Node2" : (node1 * node2[@tNode])))
           );
         CrashTail true;
         starA
           (anyA - CrashTail true
           - WriteToMid (key == k && node == ("Node2" : (node1 * node2[@tNode])))
           )),
        ReadReq (key == k),
        [| ReadRsp (key == k && not st) |] ));
  |]

let crashTail = (allA, CrashTail true, [||])

let readRsp ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int]))
    ?l:(s = (true : [%v: bool])) =
  (allA, ReadRsp (key == k && va == x && st == s), [||])

let[@goal] no_response_but_can_still_read (k : tKey) (x : int) (y : int) =
  not
    (starA (anyA - WriteRsp (key == k && va == y));
     WriteRsp (key == k && va == x);
     starA (anyA - WriteRsp (key == k && va == y));
     ReadRsp (key == k && va == y && (not (x == y)) && st);
     allA)
