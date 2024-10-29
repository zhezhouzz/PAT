(* type tNode = (node1 * node2[@tNode]) *)

val ( == ) : 'a -> 'a -> bool

(* val eStart : unit [@@gen] *)
val eBecomeLeader : < leader : (node1 * node2[@tNode]) > [@@obs]
val eClientPut : < va : tVal > [@@gen]
val eAppendEntry : < node : (node1 * node2[@tNode]) ; va : tVal > [@@obs]
val eShutDown : unit [@@gen]
val eTimeout : < dst : (node1 * node2[@tNode]) > [@@obs]

val eVoteReq :
  < src : (node1 * node2[@tNode])
  ; dst : (node1 * node2[@tNode])
  ; leader : (node1 * node2[@tNode]) >
[@@obs]

val eVoteRsp :
  < src : (node1 * node2[@tNode])
  ; dst : (node1 * node2[@tNode])
  ; stat : bool >
[@@obs]

(* let eStart = (starA (anyA - EShutDown true), EStart true, [||]) *)

let eClientPut =
  [|
    (fun ?l:(x = (true : [%v: tVal])) ->
      ( starA (anyA - EShutDown true - EBecomeLeader true),
        EClientPut (va == x),
        [|
          EAppendEntry (node == ("Node1" : (node1 * node2[@tNode])) && va == x);
          EAppendEntry (node == ("Node2" : (node1 * node2[@tNode])) && va == x);
        |] ));
  |]

let eAppendEntry ?l:(n = (true : [%v: (node1 * node2[@tNode])]))
    ?l:(x = (true : [%v: tVal])) =
  (allA, EAppendEntry (node == n && va == x), [||])

let eShutDown =
  [|
    ( starA (anyA - EBecomeLeader true),
      EShutDown true,
      [|
        ETimeout (dst == ("Node1" : (node1 * node2[@tNode])));
        ETimeout (dst == ("Node2" : (node1 * node2[@tNode])));
      |] );
  |]

(* let eTimeout ?l:(ld = (true : [%v: (node1 * node2[@tNode])])) *)
(*     ?l:(d = (not (v == ld) : [%v: (node1 * node2[@tNode])])) = *)
(*   ( starA (anyA - EBecomeLeader true), *)
(*     ETimeout (leader == ld && dst == d), *)
(*     [| *)
(*       EVoteReq (src == d && leader == d && (not (dst == d)) && not (dst == ld)); *)
(*     |] ) *)

let eTimeout =
  [|
    (fun ?l:(d =
             (v == ("Node2" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])])) ->
      ( starA (anyA - EBecomeLeader true),
        ETimeout (dst == d),
        [|
          EVoteReq
            (src == d && leader == d
            && dst == ("Node1" : (node1 * node2[@tNode])));
        |] ));
    (fun ?l:(d =
             (v == ("Node1" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])])) ->
      ( starA (anyA - EBecomeLeader true),
        ETimeout (dst == d),
        [|
          EVoteReq
            (src == d && leader == d
            && dst == ("Node2" : (node1 * node2[@tNode])));
        |] ));
  |]

let eVoteReq =
  [|
    (fun ?l:(s = (true : [%v: (node1 * node2[@tNode])]))
         ?l:(d =
             (v == ("Node1" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])]))
         ?l:(ld =
             (v == ("Node2" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])])) ->
      ( starA (anyA - EBecomeLeader true),
        EVoteReq (src == s && dst == d && leader == ld),
        [| EVoteRsp (src == d && dst == s && stat) |] ));
    (fun ?l:(s = (true : [%v: (node1 * node2[@tNode])]))
         ?l:(d =
             (v == ("Node2" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])]))
         ?l:(ld =
             (v == ("Node1" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])])) ->
      ( starA (anyA - EBecomeLeader true),
        EVoteReq (src == s && dst == d && leader == ld),
        [| EVoteRsp (src == d && dst == s && not stat) |] ));
  |]

let eVoteRsp ?l:(s = (true : [%v: (node1 * node2[@tNode])]))
    ?l:(d = (not (v == s) : [%v: (node1 * node2[@tNode])]))
    ?l:(st = (true : [%v: bool])) =
  ( starA (anyA - EBecomeLeader (leader == d)),
    EVoteRsp (src == s && dst == d && stat == st),
    [| EBecomeLeader (leader == d) |] )

let eBecomeLeader ?l:(ld = (true : [%v: (node1 * node2[@tNode])])) =
  (allA, EBecomeLeader (leader == ld), [||])

let[@goal] leaderLogSafety (n1 : (node1 * node2[@tNode]))
    (n2 : (node1 * node2[@tNode])) (x : tVal) =
  not
    (starA (anyA - EAppendEntry (node == n2 && va == x) - EBecomeLeader true);
     EAppendEntry (node == n1 && va == x);
     starA (anyA - EAppendEntry (node == n2 && va == x) - EBecomeLeader true);
     EBecomeLeader (leader == n1 && not (n1 == n2));
     starA
       (anyA
       - EAppendEntry (node == n2 && va == x)
       - EBecomeLeader true - EVoteReq true - EVoteRsp true);
     EBecomeLeader (leader == n2);
     starA (anyA - EBecomeLeader true - EVoteReq true - EVoteRsp true))
