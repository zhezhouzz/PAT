val ( == ) : 'a. 'a -> 'a -> bool

(* Basic Typing *)

val addEdge : < st : int ; ch : char ; ed : int > [@@gen]
val init : < > [@@gen]
val newNodeReq : < > [@@gen]
val newNodeResp : < nid : int > [@@obs]
val setInitNode : < nid : int > [@@gen]
val setFinalNode : < nid : int > [@@gen]
val isNFAReq : < > [@@gen]
val isNFAResp : < isNFA : bool > [@@obs]

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
      (starA (anyA - NewNodeResp true), NewNodeResp (nid == x), allA));
    (fun ?l:(x = (true : [%v: int])) ->
      ( (starA (anyA - NewNodeResp true);
         NewNodeResp (not (nid == x));
         starA (anyA - NewNodeResp (nid == x))),
        NewNodeResp (nid == x),
        (allA;
         AddEdge (ed == x);
         allA) ));
  |]

let setInitNode ?l:(x = (true : [%v: int])) =
  ( (allA;
     NewNodeResp (nid == x);
     allA),
    SetInitNode (nid == x),
    starA (anyA - SetInitNode true) )

let setFinalNode ?l:(x = (true : [%v: int])) =
  ( (allA;
     NewNodeResp (nid == x);
     allA),
    SetFinalNode (nid == x),
    starA (anyA - SetFinalNode true) )

let addEdge =
  [|
    (fun ?l:(x = (true : [%v: int]))
      ?l:(c = (true : [%v: char]))
      ?l:(y = (not (v == x) : [%v: int]))
    ->
      ( (allA;
         SetInitNode (nid == x);
         allA;
         NewNodeResp (nid == y);
         allA),
        AddEdge (st == x && ch == c && ed == y),
        starA (anyA - NewNodeReq true - AddEdge (st == x && ch == c && ed == y))
      ));
    (fun ?l:(x = (true : [%v: int]))
      ?l:(c = (true : [%v: char]))
      ?l:(y = (not (v == x) : [%v: int]))
    ->
      ( (allA;
         NewNodeResp (nid == y);
         allA;
         NewNodeResp (nid == x);
         allA;
         AddEdge (ed == x);
         allA),
        AddEdge (st == x && ch == c && ed == y),
        starA (anyA - NewNodeReq true - AddEdge (st == x && ch == c && ed == y))
      ));
    (fun ?l:(x = (true : [%v: int]))
      ?l:(c = (true : [%v: char]))
      ?l:(y = (not (v == x) : [%v: int]))
    ->
      ( (allA;
         NewNodeResp (nid == x);
         allA;
         NewNodeResp (nid == y);
         allA;
         AddEdge (ed == x);
         allA),
        AddEdge (st == x && ch == c && ed == y),
        starA (anyA - NewNodeReq true - AddEdge (st == x && ch == c && ed == y))
      ));
  |]

let isNFAReq =
  ( allA,
    IsNFAReq true,
    (IsNFAResp true;
     allA) )

let isNFAResp =
 fun ?l:(x = (true : [%v: bool])) ->
  ( (allA;
     SetFinalNode true;
     allA),
    IsNFAResp (isNFA == x),
    allA )

let[@goal] nfa (x : int) (c1 : char) (c2 : char) (y : int) =
  allA;
  AddEdge (st == x && ch == c1 && ed == y);
  allA;
  AddEdge (st == x && ch == c2 && ed == y);
  allA;
  IsNFAResp (isNFA == true)
