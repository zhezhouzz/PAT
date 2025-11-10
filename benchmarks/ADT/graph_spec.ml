val ( == ) : 'a. 'a -> 'a -> bool

(* Basic Typing *)

val isStart : int -> bool

let[@axiom] isStart (x : int) = iff (x == 0) (isStart x)

val addEdge : < st : int ; ed : int > [@@gen]
val init : < > [@@gen]
val newNodeReq : < > [@@gen]
val newNodeResp : < nid : int > [@@obs]
val isConnectedReq : < > [@@gen]
val isConnectedResp : < isConnected : bool > [@@obs]

(* PATs *)
let init = (epsilonA, Init true, starA (anyA - Init true))

let newNodeReq =
  ( (Init true;
     allA),
    NewNodeReq true,
    (NewNodeResp true;
     allA) )

let newNodeResp =
  [|
    (fun ?l:(x = (true : [%v: int])) ->
      ( starA (anyA - NewNodeResp true),
        NewNodeResp (nid == x && isStart x),
        allA ));
    (fun ?l:(x = (true : [%v: int])) ->
      ( (starA (anyA - NewNodeResp true);
         NewNodeResp (not (nid == x));
         starA (anyA - NewNodeResp (nid == x))),
        NewNodeResp (nid == x && not (isStart x)),
        (allA;
         AddEdge (ed == x);
         allA) ));
  |]

let addEdge =
  [|
    (fun ?l:(x = (true : [%v: int])) ?l:(y = (not (v == x) : [%v: int])) ->
      ( (allA;
         NewNodeResp (nid == x);
         allA;
         NewNodeResp (nid == y);
         allA),
        AddEdge (st == x && ed == y && not (isStart ed)),
        starA (anyA - AddEdge (st == x && ed == y)) ));
  |]

let isConnectedReq =
  ( allA,
    IsConnectedReq true,
    (IsConnectedResp true;
     allA) )

let isConnectedResp =
 fun ?l:(x = (true : [%v: bool])) ->
  (allA, IsConnectedResp (isConnected == x), allA)

(* all nodes are connected to each other *)
let[@goal] graph (x : int) (y : int) (z : int) =
  allA;
  AddEdge (st == x && ed == y && not (isStart st));
  allA;
  AddEdge (st == y && ed == z);
  allA;
  IsConnectedReq true;
  IsConnectedResp (isConnected == true)
