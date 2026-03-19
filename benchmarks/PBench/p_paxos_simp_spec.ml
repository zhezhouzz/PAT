(* Simplified from p_paxos_spec.ml
   Original: Paxos with prepare/accept phases, proposer/acceptor nodes.
   Simplified: Observation events (eLearn, eLostPrepareReq, ePrepareReq, ePrepareRsp,
   eLostAcceptReq, eAcceptReq, eLostAcceptRsp, eAcceptRsp) use qualifier true where
   the structure preserves linking. eStart two cases merged - both send to acc1 or
   acc2. pr2/acc1/acc2 refinements kept for control flow. *)
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

(* Merged: 2 cases -> 1, histories united with || *)
let eStart =
  [|
    (fun ?l:(p = (true : [%v: tProposerNode])) ?l:(x = (true : [%v: tVal])) ->
      ( (allA;
         ELostPrepareReq (proposer == p && acc2 acceptor);
         allA)
        ||
        (allA;
         ELostPrepareReq (proposer == p && acc1 acceptor);
         allA),
        EStart true,
        [|
          EPrepareReq (proposer == p && acc1 acceptor && va == x);
          EPrepareReq (proposer == p && acc2 acceptor && va == x);
        |] ));
  |]

let eLearn ?l:(x = (true : [%v: tVal])) = (allA, ELearn true, [||])

let eLostPrepareReq ?l:(p = (true : [%v: tProposerNode]))
    ?l:(ac = (true : [%v: tAcceptorNode])) =
  (allA, ELostPrepareReq true, [||])

(* Merged: cases 1 & 2 (both produce EPrepareRsp), case 3 kept separate (no response) *)
let ePrepareReq =
  [|
    (fun ?l:(p = (true : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (allA;
         EPrepareReq (pr1 proposer && acceptor == ac);
         allA)
        || starA (anyA - EPrepareReq (acceptor == ac)),
        EPrepareReq true,
        [| EPrepareRsp (acceptor == ac && promised == p && va == x) |] ));
    (fun ?l:(p = (true : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (allA;
         ELostPrepareRsp (acceptor == ac && promised == p);
         allA),
        EPrepareReq true,
        [||] ));
  |]

let eLostPrepareRsp ?l:(ac = (true : [%v: tAcceptorNode]))
    ?l:(p = (true : [%v: tProposerNode])) =
  (allA, ELostPrepareRsp true, [||])

let ePrepareRsp =
  [|
    (fun ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(p = (true : [%v: tProposerNode]))
      ?l:(x = (true : [%v: tVal]))
      ?l:(ap = (true : [%v: tProposerNode]))
    ->
      ( starA (anyA - EPrepareRsp (promised == p)),
        EPrepareRsp true,
        [| EAcceptReq (acceptor == ac && proposer == p && va == x) |] ));
    (fun ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(p = (true : [%v: tProposerNode]))
      ?l:(x = (true : [%v: tVal]))
      ?l:(ap = (true : [%v: tProposerNode]))
    ->
      ( starA (anyA - EPrepareRsp (promised == p)),
        EPrepareRsp true,
        [|
          EAcceptReq (acc1 acceptor && proposer == p && va == x);
          EAcceptReq (acc2 acceptor && proposer == p && va == x);
        |] ));
    (fun ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(p = (true : [%v: tProposerNode]))
      ?l:(x = (true : [%v: tVal]))
      ?l:(ap = (true : [%v: tProposerNode]))
    ->
      ( (allA;
         ELostAcceptReq (acceptor == ac && proposer == p);
         allA),
        EPrepareRsp true,
        [||] ));
    (fun ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(p = (true : [%v: tProposerNode]))
      ?l:(x = (true : [%v: tVal]))
      ?l:(ap = (true : [%v: tProposerNode]))
    ->
      ( allA,
        EPrepareRsp true,
        [| EAcceptReq (acceptor == ac && proposer == p && va == x) |] ));
  |]

let eLostAcceptReq ?l:(p = (true : [%v: tProposerNode]))
    ?l:(ac = (true : [%v: tAcceptorNode])) =
  (allA, ELostAcceptReq true, [||])

(* Merged: cases 1 & 2 (both produce EAcceptRsp), case 3 kept separate (no response) *)
let eAcceptReq =
  [|
    (fun ?l:(p = (true : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (allA;
         EAcceptReq (pr1 proposer && acceptor == ac);
         allA)
        || starA (anyA - EAcceptReq (acceptor == ac)),
        EAcceptReq true,
        [|
          EAcceptRsp
            (proposer == p && acceptor == ac && accepted == p && va == x);
        |] ));
    (fun ?l:(p = (true : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (allA;
         ELostAcceptRsp (proposer == p && acceptor == ac);
         allA),
        EAcceptReq true,
        [||] ));
  |]

let eLostAcceptRsp ?l:(p = (true : [%v: tProposerNode]))
    ?l:(ac = (true : [%v: tAcceptorNode])) =
  (allA, ELostAcceptRsp true, [||])

(* Merged: 2 cases -> 1, histories united with || *)
let eAcceptRsp =
  [|
    (fun ?l:(p = (true : [%v: tProposerNode]))
      ?l:(ac = (true : [%v: tAcceptorNode]))
      ?l:(ap = (v == p : [%v: tProposerNode]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (starA (anyA - EPrepareRsp true);
         EAcceptRsp true;
         allA)
        || starA (anyA - EAcceptRsp (proposer == p && accepted == ap)),
        EAcceptRsp true,
        [| ELearn (va == x) |] ));
  |]

(* leaner consistent view - UNCHANGED *)
let[@goal] p_paxos (x : tVal) (y : tVal) =
  allA;
  ELearn (va == x);
  allA;
  ELearn (va == y && not (x == y));
  allA
