(* Simplified from nfa_spec.ml
   Original newNodeResp: 2 cases (like graph) - isStart vs not.
   Simplified: Merged into 1 case using SRE union; event qualifier to nid==x.
   Original addEdge: 3 cases - (1) from regular node, (2) from init node, (3) to node x.
   Simplified: Merged into 1 case using SRE union (||) for history.
   init: epsilonA preserved (must be first).
   setInitNode, setFinalNode: starA (anyA - SetInitNode/SetFinalNode true) preserved (only once). *)
val ( == ) : 'a. 'a -> 'a -> bool
val isStart : int -> bool

let[@axiom] isStart (x : int) = iff (x == 0) (isStart x)

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
      ( starA (anyA - NewNodeResp true)
        ||
        (starA (anyA - NewNodeResp true);
         NewNodeResp (not (nid == x));
         starA (anyA - NewNodeResp (nid == x))),
        NewNodeResp (nid == x),
        allA ));
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
         NewNodeResp (nid == x);
         allA;
         NewNodeResp (nid == y);
         allA;
         AddEdge (ed == x);
         allA)
        ||
        (allA;
         SetInitNode (nid == x);
         allA;
         NewNodeResp (nid == y);
         allA),
        AddEdge (st == x && ch == c && ed == y),
        allA ));
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

let[@goal] nfa =
  allA;
  AddEdge true;
  allA;
  AddEdge true;
  allA;
  AddEdge true;
  allA;
  IsNFAResp (isNFA == true)
