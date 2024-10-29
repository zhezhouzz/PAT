(* type tNode = (node1 * node2 * node3[@tNode]) *)

val ( == ) : 'a -> 'a -> bool
val eStart : unit [@@gen]
val eInitLeader : < leader : (node1 * node2 * node3[@tNode]) > [@@obs]
val eBecomeLeader : < leader : (node1 * node2 * node3[@tNode]) > [@@obs]

val eClientPut : < leader : (node1 * node2 * node3[@tNode]) ; va : tVal >
[@@obs]

val eAppendEntry : < node : (node1 * node2 * node3[@tNode]) ; va : tVal >
[@@obs]

val eLog : < node : (node1 * node2 * node3[@tNode]) ; va : tVal > [@@obs]
val eShutDown : < node : (node1 * node2 * node3[@tNode]) > [@@gen]

val eTimeout :
  < leader : (node1 * node2 * node3[@tNode])
  ; dst : (node1 * node2 * node3[@tNode]) >
[@@gen]

val eVoteReq :
  < src : (node1 * node2 * node3[@tNode])
  ; dst : (node1 * node2 * node3[@tNode])
  ; leader : (node1 * node2 * node3[@tNode]) >
[@@gen]

val eVoteRsp :
  < src : (node1 * node2 * node3[@tNode])
  ; dst : (node1 * node2 * node3[@tNode])
  ; leader : (node1 * node2 * node3[@tNode]) >
[@@gen]

let eStart = (allA, EStart true, [| EInitLeader true |])

let eInitLeader ?l:(ld = (true : [%v: (node1 * node2 * node3[@tNode])])) =
  (allA, EInitLeader (leader == ld), [||])

let eClientPut =
  [|
    (fun ?l:(ld =
             (v == ("Node1" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(x = (true : [%v: tVal])) ->
      ( allA,
        EClientPut (leader == ld && va == x),
        [|
          EAppendEntry
            (node == ("Node2" : (node1 * node2 * node3[@tNode])) && va == x);
          EAppendEntry
            (node == ("Node3" : (node1 * node2 * node3[@tNode])) && va == x);
          ELog (node == ld && va == x);
        |] ));
    (fun ?l:(ld =
             (v == ("Node2" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(x = (true : [%v: tVal])) ->
      ( allA,
        EClientPut (leader == ld && va == x),
        [|
          EAppendEntry
            (node == ("Node3" : (node1 * node2 * node3[@tNode])) && va == x);
          EAppendEntry
            (node == ("Node1" : (node1 * node2 * node3[@tNode])) && va == x);
          ELog (node == ld && va == x);
        |] ));
    (fun ?l:(ld =
             (v == ("Node3" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(x = (true : [%v: tVal])) ->
      ( allA,
        EClientPut (leader == ld && va == x),
        [|
          EAppendEntry
            (node == ("Node1" : (node1 * node2 * node3[@tNode])) && va == x);
          EAppendEntry
            (node == ("Node2" : (node1 * node2 * node3[@tNode])) && va == x);
          ELog (node == ld && va == x);
        |] ));
  |]

let eAppendEntry ?l:(n = (true : [%v: (node1 * node2 * node3[@tNode])]))
    ?l:(x = (true : [%v: tVal])) =
  (allA, EAppendEntry (node == n && va == x), [| ELog (node == n && va == x) |])

let eLog ?l:(n = (true : [%v: (node1 * node2 * node3[@tNode])]))
    ?l:(x = (true : [%v: tVal])) =
  (allA, ELog (node == n && va == x), [||])

let eShutDown =
  [|
    (fun ?l:(n =
             (v == ("Node1" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])])) ->
      ( (allA;
         EInitLeader (leader == n);
         allA),
        EShutDown (node == n),
        [|
          ETimeout
            (leader == n && dst == ("Node2" : (node1 * node2 * node3[@tNode])));
          ETimeout
            (leader == n && dst == ("Node3" : (node1 * node2 * node3[@tNode])));
        |] ));
    (fun ?l:(n =
             (v == ("Node2" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])])) ->
      ( (allA;
         EInitLeader (leader == n);
         allA),
        EShutDown (node == n),
        [|
          ETimeout
            (leader == n && dst == ("Node3" : (node1 * node2 * node3[@tNode])));
          ETimeout
            (leader == n && dst == ("Node1" : (node1 * node2 * node3[@tNode])));
        |] ));
    (fun ?l:(n =
             (v == ("Node3" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])])) ->
      ( (allA;
         EInitLeader (leader == n);
         allA),
        EShutDown (node == n),
        [|
          ETimeout
            (leader == n && dst == ("Node1" : (node1 * node2 * node3[@tNode])));
          ETimeout
            (leader == n && dst == ("Node2" : (node1 * node2 * node3[@tNode])));
        |] ));
  |]

let eTimeout ?l:(ld = (true : [%v: (node1 * node2 * node3[@tNode])]))
    ?l:(d = (not (v == ld) : [%v: (node1 * node2 * node3[@tNode])])) =
  ( allA,
    ETimeout (leader == ld && dst == d),
    [|
      EVoteReq (src == d && leader == d && (not (dst == d)) && not (dst == ld));
    |] )

let eVoteReq =
  [|
    (fun ?l:(s = (true : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(d =
             (v == ("Node1" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(ld =
             (v == ("Node2" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])])) ->
      ( allA,
        EVoteReq (src == s && dst == d && leader == ld),
        [|
          EVoteRsp
            (src == d && dst == s
            && leader == ("Node2" : (node1 * node2 * node3[@tNode])));
        |] ));
    (fun ?l:(s = (true : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(d =
             (v == ("Node1" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(ld =
             (v == ("Node3" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])])) ->
      ( allA,
        EVoteReq (src == s && dst == d && leader == ld),
        [|
          EVoteRsp
            (src == d && dst == s
            && leader == ("Node3" : (node1 * node2 * node3[@tNode])));
        |] ));
    (fun ?l:(s = (true : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(d =
             (v == ("Node2" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(ld =
             (v == ("Node1" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])])) ->
      ( allA,
        EVoteReq (src == s && dst == d && leader == ld),
        [|
          EVoteRsp
            (src == d && dst == s
            && leader == ("Node2" : (node1 * node2 * node3[@tNode])));
        |] ));
    (fun ?l:(s = (true : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(d =
             (v == ("Node2" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(ld =
             (v == ("Node3" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])])) ->
      ( allA,
        EVoteReq (src == s && dst == d && leader == ld),
        [|
          EVoteRsp
            (src == d && dst == s
            && leader == ("Node3" : (node1 * node2 * node3[@tNode])));
        |] ));
    (fun ?l:(s = (true : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(d =
             (v == ("Node3" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(ld =
             (v == ("Node1" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])])) ->
      ( allA,
        EVoteReq (src == s && dst == d && leader == ld),
        [|
          EVoteRsp
            (src == d && dst == s
            && leader == ("Node3" : (node1 * node2 * node3[@tNode])));
        |] ));
    (fun ?l:(s = (true : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(d =
             (v == ("Node3" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])]))
         ?l:(ld =
             (v == ("Node2" : (node1 * node2 * node3[@tNode]))
               : [%v: (node1 * node2 * node3[@tNode])])) ->
      ( allA,
        EVoteReq (src == s && dst == d && leader == ld),
        [|
          EVoteRsp
            (src == d && dst == s
            && leader == ("Node3" : (node1 * node2 * node3[@tNode])));
        |] ));
  |]

let eVoteRsp ?l:(s = (true : [%v: (node1 * node2 * node3[@tNode])]))
    ?l:(d = (not (v == s) : [%v: (node1 * node2 * node3[@tNode])]))
    ?l:(ld = (true : [%v: (node1 * node2 * node3[@tNode])])) =
  ( allA,
    EVoteRsp (src == s && dst == d && leader == ld),
    [| EBecomeLeader (leader == d) |] )

let eBecomeLeader ?l:(ld = (true : [%v: (node1 * node2 * node3[@tNode])])) =
  (allA, EBecomeLeader (leader == ld), [||])

let[@goal] leaderLogSafety (n1 : (node1 * node2 * node3[@tNode]))
    (n2 : (node1 * node2 * node3[@tNode])) (x : tVal) =
  not
    (starA (anyA - ELog (node == n2 && va == x));
     ELog (node == n1 && va == x);
     starA (anyA - EShutDown true - ELog (node == n2 && va == x));
     EBecomeLeader (leader == n1);
     starA (anyA - EShutDown true - ELog (node == n2 && va == x));
     EBecomeLeader (leader == n2 && not (n1 == n2));
     starA (anyA - EShutDown true))
