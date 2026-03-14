(* type tNode = tNode *)

val ( == ) : 'a. 'a -> 'a -> bool
val ( < ) : int -> int -> bool
val nodeId : tNode -> int
val next : tNode -> tNode

let[@axiom] node_id_unique (n1 : tNode) (n2 : tNode) =
  iff (nodeId n1 == nodeId n2) (n1 == n2)

let[@axiom] node_two (n : tNode) = 1 == nodeId n || 2 == nodeId n

let[@axiom] node_next_1 (n1 : tNode) =
  iff (nodeId n1 == 1) (nodeId (next n1) == 2)

let[@axiom] node_next_2 (n1 : tNode) =
  iff (nodeId n1 == 2) (nodeId (next n1) == 1)

val eStart : < > [@@gen]
val eBecomeLeader : < leader : tNode > [@@obs]
val eClientPut : < va : tVal > [@@gen]
val eClientPutRsp : < va : tVal ; stat : bool > [@@gen]
val eAppendEntry : < node : tNode ; va : tVal > [@@obs]
val eShutDown : < > [@@gen]
val eTimeout : < dest : tNode > [@@obs]
val eVoteReq : < src : tNode ; dest : tNode ; leader : tNode > [@@obs]
val eVoteRsp : < src : tNode ; dest : tNode ; stat : bool > [@@obs]

let eStart = (starA (anyA - EShutDown true), EStart true, [||])

let eClientPutRsp ?l:(x = (true : [%v: tVal])) ?l:(st = (true : [%v: bool])) =
  (allA, EClientPutRsp (va == x && stat == st), [||])

let eClientPut =
  [|
    (fun ?l:(x = (true : [%v: tVal])) ->
      ( allA,
        EClientPut (va == x),
        [|
          EAppendEntry (nodeId node == 1 && va == x);
          EAppendEntry (nodeId node == 2 && va == x);
        |] ));
  |]

let eAppendEntry ?l:(n = (true : [%v: tNode])) ?l:(x = (true : [%v: tVal])) =
  (allA, EAppendEntry (node == n && va == x), [||])

let eShutDown =
  [|
    ( allA,
      EShutDown true,
      [| ETimeout (nodeId dest == 1); ETimeout (nodeId dest == 2) |] );
  |]

let eTimeout =
  [|
    (fun ?l:(d = (true : [%v: tNode])) ->
      ( allA,
        ETimeout (dest == d),
        [| EVoteReq (src == d && leader == d && dest == next d) |] ));
  |]

let eVoteReq =
  [|
    (fun ?l:(s = (true : [%v: tNode]))
      ?l:(d = (true : [%v: tNode]))
      ?l:(ld = (v == d : [%v: tNode]))
    ->
      ( allA,
        EVoteReq (src == s && dest == d && leader == ld),
        [| EVoteRsp (src == d && dest == s && not stat) |] ));
    (fun ?l:(s = (true : [%v: tNode]))
      ?l:(d = (true : [%v: tNode]))
      ?l:(ld = (not (v == d) : [%v: tNode]))
    ->
      ( allA,
        EVoteReq (src == s && dest == d && leader == ld),
        [| EVoteRsp (src == d && dest == s && stat == (nodeId d < nodeId ld)) |]
      ));
  |]

let eVoteRsp ?l:(s = (true : [%v: tNode])) ?l:(d = (not (v == s) : [%v: tNode]))
    ?l:(st = (true : [%v: bool])) =
  ( allA,
    EVoteRsp (src == s && dest == d && stat == st),
    [| EBecomeLeader (leader == d) |] )

let eBecomeLeader ?l:(ld = (true : [%v: tNode])) =
  (allA, EBecomeLeader (leader == ld), [||])

(* leader log safety *)
let[@goal] task_Raft (n1 : tNode) (n2 : tNode) (x : tVal) =
  allA;
  EAppendEntry (node == n1 && va == x);
  allA;
  EBecomeLeader (leader == n1 && not (n1 == n2));
  allA;
  EBecomeLeader (leader == n2);
  starA (anyA - EBecomeLeader true - EVoteReq true - EVoteRsp true)

(* leader log safety *)
(* let[@goal] task_Raft (n1 : tNode) (n2 : tNode) (x : tVal) =
  starA (anyA - EAppendEntry (node == n2 && va == x) - EBecomeLeader true);
  EAppendEntry (node == n1 && va == x);
  starA (anyA - EAppendEntry (node == n2 && va == x) - EBecomeLeader true);
  EBecomeLeader (leader == n1 && not (n1 == n2));
  starA
    (anyA
    - EAppendEntry (node == n2 && va == x)
    - EBecomeLeader true - EVoteReq true - EVoteRsp true);
  EBecomeLeader (leader == n2);
  starA (anyA - EBecomeLeader true - EVoteReq true - EVoteRsp true) *)
