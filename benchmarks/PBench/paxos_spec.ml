(* type tAcceptorNode = tAcceptorNode *)
(* type tProposerNode = tProposerNode *)

val ( == ) : 'a. 'a -> 'a -> bool
val acc1 : tAcceptorNode -> bool
val acc2 : tAcceptorNode -> bool
val pr1 : tProposerNode -> bool
val pr2 : tProposerNode -> bool

let[@axiom] pr1_is_not_pr2 (p1 : tProposerNode) = not (pr1 p1 && pr2 p1)
let[@axiom] pr1_or_pr2 (p1 : tProposerNode) = pr1 p1 || pr2 p1
let[@axiom] acc1_is_not_acc2 (a1 : tAcceptorNode) = not (acc1 a1 && acc2 a1)
let[@axiom] acc1_or_acc2 (a1 : tAcceptorNode) = acc1 a1 || acc2 a1

val eStart : < proposer : tProposerNode ; va : tVal > [@@gen]

val eLostPrepareReq : < proposer : tProposerNode ; acceptor : tAcceptorNode >
[@@gen]

val ePrepareReq :
  < proposer : tProposerNode ; acceptor : tAcceptorNode ; va : tVal >
[@@obs]

val eLostPrepareRsp : < acceptor : tAcceptorNode ; promised : tProposerNode >
[@@gen]

val ePrepareRsp :
  < acceptor : tAcceptorNode
  ; promised : tProposerNode
  ; va : tVal
  ; n_accepted : tProposerNode >
[@@obs]

val eLostAcceptReq : < proposer : tProposerNode ; acceptor : tAcceptorNode >
[@@gen]

val eAcceptReq :
  < proposer : tProposerNode ; acceptor : tAcceptorNode ; va : tVal >
[@@obs]

val eLostAcceptRsp : < proposer : tProposerNode ; acceptor : tAcceptorNode >
[@@gen]

val eAcceptRsp :
  < proposer : tProposerNode
  ; acceptor : tAcceptorNode
  ; accepted : tProposerNode
  ; va : tVal >
[@@obs]

val eLearn : < va : tVal > [@@obs]

let eStart =
  [|
    (fun ?l:(p = (true : [%v: tProposerNode])) ?l:(x = (true : [%v: tVal])) ->
      ( (allA;
         ELostPrepareReq (proposer == p && acc2 acceptor);
         allA),
        EStart (proposer == p && va == x),
        [| EPrepareReq (proposer == p && acc1 acceptor && va == x) |] ));
    (fun ?l:(p = (true : [%v: tProposerNode])) ?l:(x = (true : [%v: tVal])) ->
      ( (allA;
         ELostPrepareReq (proposer == p && acc1 acceptor);
         allA),
        EStart (proposer == p && va == x),
        [| EPrepareReq (proposer == p && acc2 acceptor && va == x) |] ));
  |]

let eLearn ?l:(x = (true : [%v: tVal])) = (allA, ELearn (va == x), [||])

let eLostPrepareReq ?l:(p = (true : [%v: tProposerNode]))
    ?l:(ac = (true : [%v: tAcceptorNode])) =
  (allA, ELostPrepareReq (proposer == p && acceptor == ac), [||])

let ePrepareReq =
  [|
    (* When there is a previous request less than proposer, accept it. *)
    (fun ?l:(p = (pr2 v : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (starA (anyA - EPrepareReq (acceptor == ac) - EAcceptReq true);
         EPrepareReq (pr1 proposer && acceptor == ac);
         starA (anyA - EPrepareReq (acceptor == ac) - EAcceptReq true)),
        EPrepareReq (proposer == p && acceptor == ac && va == x),
        [| EPrepareRsp (acceptor == ac && promised == p && va == x) |] ));
    (* When there no previous prepare request, accept it. *)
    (fun ?l:(p = (true : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( starA (anyA - EPrepareReq (acceptor == ac) - EAcceptReq true),
        EPrepareReq (proposer == p && acceptor == ac && va == x),
        [| EPrepareRsp (acceptor == ac && promised == p && va == x) |] ));
    (* We can always don't reply. *)
    (fun ?l:(p = (true : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (allA;
         ELostPrepareRsp (acceptor == ac && promised == p);
         allA),
        EPrepareReq (proposer == p && acceptor == ac && va == x),
        [||] ));
  |]

let eLostPrepareRsp ?l:(ac = (true : [%v: tAcceptorNode]))
    ?l:(p = (true : [%v: tProposerNode])) =
  (allA, ELostPrepareRsp (acceptor == ac && promised == p), [||])

let ePrepareRsp =
  [|
    (fun ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(p = (true : [%v: tProposerNode]))
      ?l:(x = (true : [%v: tVal]))
      ?l:(ap = (true : [%v: tProposerNode]))
    ->
      ( starA (anyA - EPrepareRsp (promised == p)),
        EPrepareRsp
          (acceptor == ac && promised == p && va == x && n_accepted == ap),
        [| EAcceptReq (acceptor == ac && proposer == p && va == x) |] ));
    (fun ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(p = (true : [%v: tProposerNode]))
      ?l:(x = (true : [%v: tVal]))
      ?l:(ap = (true : [%v: tProposerNode]))
    ->
      ( starA (anyA - EPrepareRsp (promised == p)),
        EPrepareRsp
          (acceptor == ac && promised == p && va == x && n_accepted == ap),
        [|
          EAcceptReq (acc1 acceptor && proposer == p && va == x);
          EAcceptReq (acc2 acceptor && proposer == p && va == x);
        |] ));
    (* We can always don't reply. *)
    (fun ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(p = (true : [%v: tProposerNode]))
      ?l:(x = (true : [%v: tVal]))
      ?l:(ap = (true : [%v: tProposerNode]))
    ->
      ( (allA;
         ELostAcceptReq (acceptor == ac && proposer == p);
         allA),
        EPrepareRsp
          (acceptor == ac && promised == p && va == x && n_accepted == ap),
        [||] ));
    (* When is the first response recieved, goto accept state and send accept request. (omit the second one) *)
    (fun ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(p = (true : [%v: tProposerNode]))
      ?l:(x = (true : [%v: tVal]))
      ?l:(ap = (true : [%v: tProposerNode]))
    ->
      ( starA (anyA - EPrepareRsp (promised == p)),
        EPrepareRsp
          (acceptor == ac && promised == p && va == x && n_accepted == ap),
        [| EAcceptReq (acceptor == ac && proposer == p && va == x) |] ));
  |]

let eLostAcceptReq ?l:(p = (true : [%v: tProposerNode]))
    ?l:(ac = (true : [%v: tAcceptorNode])) =
  (allA, ELostAcceptReq (proposer == p && acceptor == ac), [||])

let eAcceptReq =
  [|
    (* When there is a previous request less than proposer, accept it. *)
    (fun ?l:(p = (pr2 v : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (starA (anyA - EAcceptReq (acceptor == ac));
         EAcceptReq (pr1 proposer && acceptor == ac);
         starA (anyA - EAcceptReq (acceptor == ac))),
        EAcceptReq (proposer == p && acceptor == ac && va == x),
        [|
          EAcceptRsp
            (proposer == p && acceptor == ac && accepted == p && va == x);
        |] ));
    (* When is the first accept request recieved, accept it. *)
    (fun ?l:(p = (true : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( starA (anyA - EAcceptReq (acceptor == ac)),
        EAcceptReq (proposer == p && acceptor == ac && va == x),
        [|
          EAcceptRsp
            (proposer == p && acceptor == ac && accepted == p && va == x);
        |] ));
    (* We can always don't reply. *)
    (fun ?l:(p = (true : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (allA;
         ELostAcceptRsp (proposer == p && acceptor == ac);
         allA),
        EAcceptReq (proposer == p && acceptor == ac && va == x),
        [||] ));
  |]

let eLostAcceptRsp ?l:(p = (true : [%v: tProposerNode]))
    ?l:(ac = (true : [%v: tAcceptorNode])) =
  (allA, ELostAcceptRsp (proposer == p && acceptor == ac), [||])

let eAcceptRsp =
  [|
    (* When is the first response recieved, also equal it itself send to leaner. (omit the second one) *)
    (fun ?l:(p = (true : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(ap = (v == p : [%v: tProposerNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (starA (anyA - EPrepareRsp true);
         EAcceptRsp (proposer == p && accepted == ap);
         allA),
        EAcceptRsp (proposer == p && acceptor == ac && accepted == ap && va == x),
        [| ELearn (va == x) |] ));
    (* When is the first response recieved, also equal it itself send to leaner. (omit the second one) *)
    (fun ?l:(p = (true : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(ap = (v == p : [%v: tProposerNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( starA (anyA - EAcceptRsp (proposer == p && accepted == ap)),
        EAcceptRsp (proposer == p && acceptor == ac && accepted == ap && va == x),
        [| ELearn (va == x) |] ));
  |]

(* leaner consistent view *)

let[@goal] task_Paxos (x : tVal) (y : tVal) =
  allA;
  ELearn (va == x);
  allA;
  ELearn (va == y && not (x == y));
  allA
