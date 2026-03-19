(* Simplified from p_chainreplication_spec.ml
   Original: Chain replication with write to mid/tail, crash, read. 5 readReq cases.
   Simplified: Merged readReq intersection types using SRE union (||):
   - Failure case: merged case 1 (no WriteToTail) and case 4 (crash before WriteToMid)
   - Success case: merged case 2 (crash before WriteToMid), case 3 (crash after WriteToMid),
     and case 5 (WriteToTail) into one with union of histories.
   Qualifiers preserved (no simplification to true). *)
val ( == ) : 'a. 'a -> 'a -> bool
val mid1 : tNode -> bool
val mid2 : tNode -> bool
val next : tNode -> tNode

let[@axiom] mid_next (n1 : tNode) = (mid1 n1)#==>(mid2 (next n1))
let[@axiom] mid1_is_not_mid2 (n1 : tNode) = not (mid1 n1 && mid2 n1)

val writeReq : < key : tKey ; va : int > [@@gen]
val writeToMid : < key : tKey ; va : int ; node : tNode > [@@obs]
val writeToTail : < key : tKey ; va : int > [@@obs]
val writeRsp : < key : tKey ; va : int > [@@obsRecv]
val readReq : < key : tKey > [@@gen]
val readRsp : < key : tKey ; va : int ; st : bool > [@@obsRecv]
val crashTail : < > [@@gen]

let writeReq ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int])) =
  ( allA,
    WriteReq (key == k && va == x),
    [| WriteToMid (key == k && va == x && mid1 node) |] )

let writeToMid =
  [|
    (fun ?l:(k = (true : [%v: tKey]))
      ?l:(x = (true : [%v: int]))
      ?l:(n = (mid2 v : [%v: tNode]))
    ->
      ( allA,
        WriteToMid (key == k && va == x && node == n),
        [| WriteToTail (key == k && va == x) |] ));
    (fun ?l:(k = (true : [%v: tKey]))
      ?l:(x = (true : [%v: int]))
      ?l:(n = (mid1 v : [%v: tNode]))
    ->
      ( allA,
        WriteToMid (key == k && va == x && node == n),
        [| WriteToMid (key == k && va == x && node == next n) |] ));
  |]

let writeToTail =
  [|
    (fun ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int])) ->
      ( (starA (anyA - CrashTail true);
         CrashTail true;
         starA (anyA - CrashTail true)),
        WriteToTail (key == k && va == x),
        [||] ));
    (fun ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int])) ->
      ( starA (anyA - CrashTail true),
        WriteToTail (key == k && va == x),
        [| WriteRsp (key == k && va == x) |] ));
  |]

let writeRsp ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int])) =
  (allA, WriteRsp (key == k && va == x), [||])

(* Merged: 2 cases (failure || success) instead of 5 *)
let readReq =
  [|
    (* Failure: no WriteToTail for k, or crash before WriteToMid reaches mid2 *)
    (fun ?l:(k = (true : [%v: tKey])) ->
      ( ( starA (anyA - CrashTail true - WriteToTail (key == k))
        || ( starA (anyA - CrashTail true - WriteToMid (key == k && mid2 node));
             CrashTail true;
             starA (anyA - CrashTail true - WriteToMid (key == k && mid2 node)) )),
        ReadReq (key == k),
        [| ReadRsp (key == k && not st) |] ));
    (* Success: crash before WriteToMid | crash after WriteToMid | WriteToTail *)
    (fun (x : int) ?l:(k = (true : [%v: tKey])) ->
      ( ( (starA (anyA - CrashTail true);
          CrashTail true;
          starA (anyA - CrashTail true);
          WriteToMid (key == k && va == x && mid2 node);
          starA (anyA - CrashTail true - WriteToMid (key == k && mid2 node)))
        || ( (starA (anyA - CrashTail true);
             WriteToMid (key == k && va == x && mid2 node);
             starA (anyA - CrashTail true - WriteToMid (key == k && mid2 node));
             CrashTail true;
             starA (anyA - CrashTail true - WriteToMid (key == k && mid2 node)))
        || ( starA (anyA - CrashTail true);
            WriteToTail (key == k && va == x);
            starA (anyA - CrashTail true - WriteToTail true) ) ) ),
        ReadReq (key == k),
        [| ReadRsp (key == k && va == x && st) |] ));
  |]

let crashTail = (allA, CrashTail true, [||])

let readRsp ?l:(k = (true : [%v: tKey])) ?l:(x = (true : [%v: int]))
    ?l:(s = (true : [%v: bool])) =
  (allA, ReadRsp (key == k && va == x && st == s), [||])

(* UNCHANGED *)
let[@goal] p_chainreplication (k : tKey) (x : int) (y : int) =
  allA;
  WriteReq (key == k && va == x);
  starA (anyA - WriteRsp true);
  ReadRsp (key == k && va == y && (not (x == y)) && st);
  starA (anyA - WriteRsp true)
