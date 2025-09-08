val ( == ) : 'a. 'a -> 'a -> bool

(* Basic Typing *)

val addEdge : < st : int ; ed : int > [@@gen]
val init : < > [@@gen]
val newEdgeReq : < > [@@gen]
val newEdgeResp : < nid : int > [@@obs]
val isConnectedReq : < > [@@gen]
val isConnectedResp : < isConnected : bool > [@@obs]

(* PATs *)
let init = (epsilonA, Init true, starA (anyA - Init true))

let newEdgeReq =
  ( (Init true;
     allA),
    NewEdgeReq true,
    (NewEdgeResp true;
     allA) )

let newEdgeResp =
  [|
    (fun ?l:(x = (true : [%v: int])) ->
      (starA (anyA - NewEdgeResp true), NewEdgeResp (nid == x), [||]));
    (fun ?l:(x = (true : [%v: int])) ->
      ( (starA (anyA - NewEdgeResp (nid == x));
         NewEdgeResp true;
         starA (anyA - NewEdgeResp (nid == x))),
        NewEdgeResp (nid == x),
        [| AddEdge (ed == x) |] ));
  |]

let addEdge =
  [|
    (fun ?l:(x = (true : [%v: int])) ?l:(y = (not (v == x) : [%v: int])) ->
      ( (starA (anyA - NewEdgeResp true);
         NewEdgeResp (nid == x);
         allA;
         NewEdgeResp (nid == y);
         allA),
        AddEdge (st == x && ed == y),
        starA (anyA - NewEdgeReq true - AddEdge (st == x && ed == y)) ));
    (fun ?l:(x = (true : [%v: int])) ?l:(y = (not (v == x) : [%v: int])) ->
      ( (allA;
         NewEdgeResp (nid == y);
         allA;
         NewEdgeResp (nid == x);
         allA;
         AddEdge (ed == x);
         allA),
        AddEdge (st == x && ed == y),
        starA (anyA - NewEdgeReq true - AddEdge (st == x && ed == y)) ));
    (fun ?l:(x = (true : [%v: int])) ?l:(y = (not (v == x) : [%v: int])) ->
      ( (allA;
         NewEdgeResp (nid == x);
         allA;
         AddEdge (ed == x);
         allA;
         NewEdgeResp (nid == y);
         allA),
        AddEdge (st == x && ed == y),
        starA (anyA - NewEdgeReq true - AddEdge (st == x && ed == y)) ));
    (fun ?l:(x = (true : [%v: int])) ?l:(y = (not (v == x) : [%v: int])) ->
      ( (allA;
         NewEdgeResp (nid == x);
         allA;
         NewEdgeResp (nid == y);
         allA;
         AddEdge (ed == x);
         allA),
        AddEdge (st == x && ed == y),
        starA (anyA - NewEdgeReq true - AddEdge (st == x && ed == y)) ));
  |]

let isConnectedReq =
  ( allA,
    IsConnectedReq true,
    (IsConnectedResp true;
     allA) )

let isConnectedResp =
 fun ?l:(x = (true : [%v: bool])) ->
  (allA, IsConnectedResp (isConnected == x), allA)

let[@goal] isConnected (x : int) (y : int) =
  allA;
  NewEdgeResp (nid == x);
  allA;
  NewEdgeResp (nid == y);
  allA;
  IsConnectedReq true;
  IsConnectedResp (isConnected == true)
