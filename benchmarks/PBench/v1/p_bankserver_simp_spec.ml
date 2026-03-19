(* Simplified from p_bankserver_spec.ml
   Original: Bank withdraw with balance checks, account init (once per account).
   Simplified: Observation events (eWithDrawResp, eUpdateQuery, eReadQueryResp) use
   qualifier true. eInitAccount "只能发生一次" preserved. eReadQueryResp refinement
   (v > 0 && v > am) / (v > 0 && not (v > am)) simplified to (v > 0 : [%v: int]) for
   success case - balance must be positive; failure case kept for control flow. *)
val ( == ) : 'a. 'a -> 'a -> bool
val ( > ) : int -> int -> bool
val ( - ) : int -> int -> int
val eInitAccount : < accountId : aid ; balance : int > [@@gen]

let eInitAccount ?l:(ac = (true : [%v: aid])) ?l:(ba = (v > 0 : [%v: int])) =
  ( starA (anyA - EInitAccount (accountId == ac)),
    EInitAccount (accountId == ac && balance == ba),
    [||] )

val eWithDrawReq : < rId : rid ; accountId : aid ; amount : int > [@@gen]

let eWithDrawReq ?l:(id = (true : [%v: rid])) ?l:(ac = (true : [%v: aid]))
    ?l:(am = (v > 0 : [%v: int])) =
  ( allA,
    EWithDrawReq (rId == id && accountId == ac && amount == am),
    [| EReadQuery (rId == id && accountId == ac && amount == am) |] )

val eWithDrawResp :
  < rId : rid ; accountId : aid ; balance : int ; status : bool >
[@@obs]

let eWithDrawResp ?l:(id = (true : [%v: rid])) ?l:(ac = (true : [%v: aid]))
    ?l:(ba = (true : [%v: int])) ?l:(st = (true : [%v: bool])) =
  ( allA,
    EWithDrawResp true,
    [||] )

val eUpdateQuery : < accountId : aid ; balance : int > [@@obs]

let eUpdateQuery ?l:(ac = (true : [%v: aid])) ?l:(ba = (v > 0 : [%v: int])) =
  (allA, EUpdateQuery true, [||])

val eReadQuery : < rId : rid ; amount : int ; accountId : aid > [@@obs]

let eReadQuery =
  [|
    (fun (ba : int)
      ?l:(id = (true : [%v: rid]))
      ?l:(am = (true : [%v: int]))
      ?l:(ac = (true : [%v: aid]))
    ->
      ( (allA;
         EInitAccount (accountId == ac && balance == ba);
         starA
           (anyA - EReadQuery (accountId == ac) - EInitAccount (accountId == ac))),
        EReadQuery (rId == id && amount == am && accountId == ac),
        [|
          EReadQueryResp
            (rId == id && amount == am && accountId == ac && balance == ba);
        |] ));
  |]

val eReadQueryResp :
  < rId : rid ; amount : int ; accountId : aid ; balance : int >
[@@obs]

let eReadQueryResp =
  [|
    (fun (id : rid)
      (am : int)
      ?l:(ac = (true : [%v: aid]))
      ?l:(ba = (v > 0 : [%v: int]))
    ->
      ( (starA (anyA - EReadQueryResp true - EReadQuery true);
         EWithDrawReq (rId == id && accountId == ac && amount == am);
         starA (anyA - EWithDrawReq true)),
        EReadQueryResp true,
        [|
          EUpdateQuery (accountId == ac && balance == ba - am);
          EWithDrawResp
            (rId == id && accountId == ac && balance == ba - am && status);
        |] ));
    (fun ?l:(id = (true : [%v: rid]))
      ?l:(am = (true : [%v: int]))
      ?l:(ac = (true : [%v: aid]))
      ?l:(ba = (v > 0 && not (v > am) : [%v: int]))
    ->
      ( allA,
        EReadQueryResp true,
        [| EWithDrawResp (accountId == ac && not status) |] ));
  |]

(* bank withdraw success - UNCHANGED *)
let[@goal] p_bankserver (ac : aid) =
  allA;
  EWithDrawResp (accountId == ac && not status);
  allA
