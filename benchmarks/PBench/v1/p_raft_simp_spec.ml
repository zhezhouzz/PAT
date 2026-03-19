(* Simplified from p_raft_spec.ml
   Original: Raft leader election with append, vote, become leader.
   Simplified: Observation events (eClientPutRsp, eAppendEntry, eVoteReq, eVoteRsp,
   eBecomeLeader) use qualifier true. eStart, eShutDown "只能在发生一次" preserved.
   eClientPut, eTimeout, eVoteReq cases kept for control flow. *)
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
  (allA, EClientPutRsp true, [||])

let eClientPut =
  [|
    (fun ?l:(x = (true : [%v: tVal])) ->
      ( allA,
        EClientPut true,
        [|
          EAppendEntry (nodeId node == 1 && va == x);
          EAppendEntry (nodeId node == 2 && va == x);
        |] ));
  |]

let eAppendEntry ?l:(n = (true : [%v: tNode])) ?l:(x = (true : [%v: tVal])) =
  (allA, EAppendEntry true, [||])

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
        ETimeout true,
        [| EVoteReq (src == d && leader == d && dest == next d) |] ));
  |]

let eVoteReq =
  [|
    (fun ?l:(s = (true : [%v: tNode]))
      ?l:(d = (true : [%v: tNode]))
      ?l:(ld = (v == d : [%v: tNode]))
    ->
      ( allA,
        EVoteReq true,
        [| EVoteRsp (src == d && dest == s && not stat) |] ));
    (fun ?l:(s = (true : [%v: tNode]))
      ?l:(d = (true : [%v: tNode]))
      ?l:(ld = (not (v == d) : [%v: tNode]))
    ->
      ( allA,
        EVoteReq true,
        [| EVoteRsp (src == d && dest == s && stat == (nodeId d < nodeId ld)) |]
      ));
  |]

let eVoteRsp ?l:(s = (true : [%v: tNode])) ?l:(d = (not (v == s) : [%v: tNode]))
    ?l:(st = (true : [%v: bool])) =
  ( allA,
    EVoteRsp true,
    [| EBecomeLeader (leader == d) |] )

let eBecomeLeader ?l:(ld = (true : [%v: tNode])) =
  (allA, EBecomeLeader true, [||])

(* leader log safety - UNCHANGED *)
let[@goal] p_raft (n1 : tNode) (n2 : tNode) (x : tVal) =
  allA;
  EAppendEntry (node == n1 && va == x);
  allA;
  EBecomeLeader (leader == n1 && not (n1 == n2));
  allA;
  EBecomeLeader (leader == n2);
  starA (anyA - EBecomeLeader true - EVoteReq true - EVoteRsp true)
