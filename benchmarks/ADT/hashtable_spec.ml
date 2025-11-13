val ( == ) : 'a. 'a -> 'a -> bool
val initTblReq : < > [@@gen]
val addReq : < key : int ; value : int > [@@gen]
val findReq : < key : int > [@@gen]
val findResp : < key : int ; value : int > [@@obs]
val removeReq : < key : int > [@@gen]
val clearReq : < > [@@gen]
val findAllReq : < key : int > [@@gen]
val findAllResp : < value : int list > [@@obs]
val replaceReq : < key : int ; value : int > [@@gen]
val memReq : < key : int > [@@gen]
val memResp : < exists : bool > [@@obs]
val lengthReq : < > [@@gen]
val lengthResp : < len : int > [@@obs]

let addReq ?l:(k = (true : [%v: int])) ?l:(v = (true : [%v: int])) =
  ( (InitTblReq true;
     allA),
    AddReq (key == k && value == v),
    allA )

let findReq =
  [|
    (fun (v1 : int) ?l:(k = (true : [%v: int])) ->
      ( (allA;
         ReplaceReq (key == k && value == v1);
         starA
           (anyA
           - RemoveReq (key == k)
           - ClearReq true
           - ReplaceReq (key == k)
           - AddReq (key == k))),
        FindReq (key == k),
        (allA;
         FindResp (key == k && value == v1);
         allA) ));
  |]

let findResp =
  [|
    (fun ?l:(x = (true : [%v: int])) ?l:(v = (true : [%v: int])) ->
      (allA, FindResp (key == x && value == v), allA));
  |]

let removeReq ?l:(k = (true : [%v: int])) = (allA, RemoveReq (key == k), allA)
let clearReq = (allA, ClearReq true, allA)

let memReq ?l:(k = (true : [%v: int])) =
  ( allA,
    MemReq (key == k),
    (MemResp true;
     allA) )

let memResp ?l:(z = (true : [%v: bool])) = (allA, MemResp (exists == z), allA)

let lengthReq =
  ( allA,
    LengthReq true,
    (LengthResp true;
     allA) )

let lengthResp ?(l = (true : [%v: int])) = (allA, LengthResp (len == l), allA)
let initTblReq = (epsilonA, InitTblReq true, allA)

let replaceReq ?l:(k = (true : [%v: int])) ?l:(v = (true : [%v: int])) =
  ( (allA;
     AddReq (key == k);
     starA (anyA - RemoveReq (key == k))),
    ReplaceReq (key == k && value == v),
    allA )

let findAllReq ?l:(k = (true : [%v: int])) =
  ( allA,
    FindAllReq (key == k),
    (FindAllResp true;
     allA) )

let findAllResp ?l:(vs = (true : [%v: int list])) =
  (allA, FindAllResp (value == vs), allA)

(* if we add (k,v) then any subsequent find req for k must be followed by a find resp with v *)
(* let[@goal] add_sets_value (k : int) (v : int) =
  InitTblReq true;
  allA;
  AddReq (key == k && value == v);
  starA (anyA - RemoveReq (key == k) - ClearReq true - InitTblReq true);
  FindReq (key == k);
  starA (anyA - RemoveReq (key == k) - ClearReq true - InitTblReq true);
  FindResp ((value == v) == false) *)

(* if we add (k, v1) and then replace it with (k, v2), a find operation for k must return v2 *)
let[@goal] hashtable (k : int) (v2 : int) =
  allA;
  ReplaceReq (key == k && value == v2);
  starA (anyA - AddReq (key == k) - ReplaceReq (key == k));
  FindResp (key == k && not (value == v2))
