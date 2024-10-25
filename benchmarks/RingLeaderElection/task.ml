(* type tNode = (node1 * node2[@tNode]) *)

val ( == ) : 'a -> 'a -> bool
val eWakeup : < node : (node1 * node2[@tNode]) > [@@gen]

val eNominate :
  < node : (node1 * node2[@tNode]) ; leader : (node1 * node2[@tNode]) >
[@@obs]

val eWon : < leader : (node1 * node2[@tNode]) > [@@obsRecv]

let eWakeup ?l:(n = (true : [%v: (node1 * node2[@tNode])])) =
  ( starA (anyA - EWon true),
    EWakeup (node == n),
    [| ENominate (node == n && leader == n) |] )

let eNominate =
  [|
    (fun ?l:(n =
             (v == ("Node1" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])]))
         ?l:(ld =
             (v == ("Node2" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])])) ->
      (allA, ENominate (leader == ld && node == n), [| EWon (leader == ld) |]));
    (fun ?l:(n =
             (v == ("Node1" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])]))
         ?l:(ld =
             (v == ("Node1" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])])) ->
      ( allA,
        ENominate (leader == ld && node == n),
        [|
          ENominate (leader == ld && node == ("Node2" : (node1 * node2[@tNode])));
        |] ));
    (fun ?l:(n =
             (v == ("Node2" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])]))
         ?l:(ld =
             (v == ("Node1" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])])) ->
      (allA, ENominate (leader == ld && node == n), [| EWon (leader == ld) |]));
    (fun ?l:(n =
             (v == ("Node2" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])]))
         ?l:(ld =
             (v == ("Node2" : (node1 * node2[@tNode]))
               : [%v: (node1 * node2[@tNode])])) ->
      ( allA,
        ENominate (leader == ld && node == n),
        [|
          ENominate (leader == ld && node == ("Node1" : (node1 * node2[@tNode])));
        |] ));
  |]

let eWon ?l:(ld = (true : [%v: (node1 * node2[@tNode])])) =
  (allA, EWon (leader == ld), [||])

let[@goal] uniqueLeader (ld : (node1 * node2[@tNode])) =
  not
    (allA;
     EWon (leader == ld);
     allA;
     EWon (not (leader == ld));
     allA)
