(* type tNode = tNode *)

val ( == ) : 'a. 'a -> 'a -> bool
val next : tNode -> tNode

let[@axiom] next2 (n : tNode) = next (next n) == n
let[@axiom] next_diff (n : tNode) = not (next n == n)

val eWakeup : < node : tNode > [@@gen]
val eNominate : < node : tNode ; leader : tNode > [@@obs]
val eWon : < leader : tNode > [@@obsRecv]

let eWakeup ?l:(n = (true : [%v: tNode])) =
  ( starA (anyA - EWon true),
    EWakeup (node == n),
    [| ENominate (node == n && leader == n) |] )

let eNominate =
  [|
    (fun ?l:(n = (true : [%v: tNode])) ?l:(ld = (not (v == n) : [%v: tNode])) ->
      (allA, ENominate (leader == ld && node == n), [| EWon (leader == ld) |]));
    (fun ?l:(n = (true : [%v: tNode])) ?l:(ld = (true : [%v: tNode])) ->
      ( allA,
        ENominate (leader == ld && node == n && n == ld),
        [| ENominate (leader == ld && node == next n) |] ));
  |]

let eWon ?l:(ld = (true : [%v: tNode])) = (allA, EWon (leader == ld), [||])

(* unique leader *)

let[@goal] task_RingLeaderElection (ld : tNode) =
  allA;
  EWon (leader == ld);
  allA;
  EWon (not (leader == ld));
  allA
