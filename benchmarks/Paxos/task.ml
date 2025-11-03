(* type tAcceptorNode = (acceptor1 * acceptor2[@tAcceptorNode]) *)
(* type tProposerNode = (proposer1 * proposer2[@tProposerNode]) *)

val ( == ) : 'a. 'a -> 'a -> bool

val eStart : < proposer : (proposer1 * proposer2[@tProposerNode]) ; va : tVal >
[@@gen]

val eLostPrepareReq :
  < proposer : (proposer1 * proposer2[@tProposerNode])
  ; acceptor : (acceptor1 * acceptor2[@tAcceptorNode]) >
[@@gen]

val ePrepareReq :
  < proposer : (proposer1 * proposer2[@tProposerNode])
  ; acceptor : (acceptor1 * acceptor2[@tAcceptorNode])
  ; va : tVal >
[@@obs]

val eLostPrepareRsp :
  < acceptor : (acceptor1 * acceptor2[@tAcceptorNode])
  ; promised : (proposer1 * proposer2[@tProposerNode]) >
[@@gen]

val ePrepareRsp :
  < acceptor : (acceptor1 * acceptor2[@tAcceptorNode])
  ; promised : (proposer1 * proposer2[@tProposerNode])
  ; va : tVal
  ; n_accepted : (proposer1 * proposer2[@tProposerNode]) >
[@@obs]

val eLostAcceptReq :
  < proposer : (proposer1 * proposer2[@tProposerNode])
  ; acceptor : (acceptor1 * acceptor2[@tAcceptorNode]) >
[@@gen]

val eAcceptReq :
  < proposer : (proposer1 * proposer2[@tProposerNode])
  ; acceptor : (acceptor1 * acceptor2[@tAcceptorNode])
  ; va : tVal >
[@@obs]

val eLostAcceptRsp :
  < proposer : (proposer1 * proposer2[@tProposerNode])
  ; acceptor : (acceptor1 * acceptor2[@tAcceptorNode]) >
[@@gen]

val eAcceptRsp :
  < proposer : (proposer1 * proposer2[@tProposerNode])
  ; acceptor : (acceptor1 * acceptor2[@tAcceptorNode])
  ; accepted : (proposer1 * proposer2[@tProposerNode])
  ; va : tVal >
[@@obs]

val eLearn : < va : tVal > [@@obs]

let eStart =
  [|
    (fun ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (allA;
         ELostPrepareReq
           (proposer == p
           && acceptor
              == ("Acceptor2" : (acceptor1 * acceptor2[@tAcceptorNode])));
         allA),
        EStart (proposer == p && va == x),
        [|
          EPrepareReq
            (proposer == p
            && acceptor
               == ("Acceptor1" : (acceptor1 * acceptor2[@tAcceptorNode]))
            && va == x);
        |] ));
    (fun ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (allA;
         ELostPrepareReq
           (proposer == p
           && acceptor
              == ("Acceptor1" : (acceptor1 * acceptor2[@tAcceptorNode])));
         allA),
        EStart (proposer == p && va == x),
        [|
          EPrepareReq
            (proposer == p
            && acceptor
               == ("Acceptor2" : (acceptor1 * acceptor2[@tAcceptorNode]))
            && va == x);
        |] ));
  |]

let eLearn ?l:(x = (true : [%v: tVal])) = (allA, ELearn (va == x), [||])

let eLostPrepareReq
    ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
    ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])])) =
  (allA, ELostPrepareReq (proposer == p && acceptor == ac), [||])

let ePrepareReq =
  [|
    (* (\* When there is a previous request less than proposer, accept it. *\) *)
    (* (fun ?l:(p = *)
    (*          (v == ("Proposer2" : (proposer1 * proposer2[@tProposerNode])) *)
    (*            : [%v: (proposer1 * proposer2[@tProposerNode])])) *)
    (*      ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])])) *)
    (*      ?l:(x = (true : [%v: tVal])) -> *)
    (*   ( (starA (anyA - EPrepareReq (acceptor == ac) - EAcceptReq true); *)
    (*      EPrepareReq *)
    (*        (proposer == ("Proposer1" : (proposer1 * proposer2[@tProposerNode])) *)
    (*        && acceptor == ac); *)
    (*      starA (anyA - EPrepareReq (acceptor == ac) - EAcceptReq true)), *)
    (*     EPrepareReq (proposer == p && acceptor == ac && va == x), *)
    (*     [| EPrepareRsp (acceptor == ac && promised == p && va == x) |] )); *)
    (* When there no previous prepare request, accept it. *)
    (fun ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
      ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( starA (anyA - EPrepareReq (acceptor == ac) - EAcceptReq true),
        EPrepareReq (proposer == p && acceptor == ac && va == x),
        [| EPrepareRsp (acceptor == ac && promised == p && va == x) |] ));
    (* We can always don't reply. *)
    (fun ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
      ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (allA;
         ELostPrepareRsp (acceptor == ac && promised == p);
         allA),
        EPrepareReq (proposer == p && acceptor == ac && va == x),
        [||] ));
  |]

let eLostPrepareRsp
    ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])]))
    ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])])) =
  (allA, ELostPrepareRsp (acceptor == ac && promised == p), [||])

let ePrepareRsp =
  [|
    (* When is the first response recieved, goto accept state and send accept request. (omit the second one) *)
    (* (fun ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])])) *)
    (*      ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])])) *)
    (*      ?l:(x = (true : [%v: tVal])) *)
    (*      ?l:(ap = (true : [%v: (proposer1 * proposer2[@tProposerNode])])) -> *)
    (*   ( starA (anyA - EPrepareRsp (promised == p)), *)
    (*     EPrepareRsp *)
    (*       (acceptor == ac && promised == p && va == x && n_accepted == ap), *)
    (*     [| *)
    (*       EAcceptReq *)
    (*         (acceptor == ("Acceptor1" : (acceptor1 * acceptor2[@tAcceptorNode])) *)
    (*         && proposer == p && va == x); *)
    (*       EAcceptReq *)
    (*         (acceptor == ("Acceptor2" : (acceptor1 * acceptor2[@tAcceptorNode])) *)
    (*         && proposer == p && va == x); *)
    (*     |] )); *)
    (fun ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])]))
      ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
      ?l:(x = (true : [%v: tVal]))
      ?l:(ap = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
    ->
      ( starA (anyA - EPrepareRsp (promised == p)),
        EPrepareRsp
          (acceptor == ac && promised == p && va == x && n_accepted == ap),
        [| EAcceptReq (acceptor == ac && proposer == p && va == x) |] ));
    (* We can always don't reply. *)
    (fun ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])]))
      ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
      ?l:(x = (true : [%v: tVal]))
      ?l:(ap = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
    ->
      ( (allA;
         ELostAcceptReq (acceptor == ac && proposer == p);
         allA),
        EPrepareRsp
          (acceptor == ac && promised == p && va == x && n_accepted == ap),
        [||] ));
  |]

let eLostAcceptReq
    ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
    ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])])) =
  (allA, ELostAcceptReq (proposer == p && acceptor == ac), [||])

let eAcceptReq =
  [|
    (* (\* When there is a previous request less than proposer, accept it. *\) *)
    (* (fun ?l:(p = *)
    (*          (v == ("Proposer2" : (proposer1 * proposer2[@tProposerNode])) *)
    (*            : [%v: (proposer1 * proposer2[@tProposerNode])])) *)
    (*      ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])])) *)
    (*      ?l:(x = (true : [%v: tVal])) -> *)
    (*   ( (starA (anyA - EAcceptReq (acceptor == ac)); *)
    (*      EAcceptReq *)
    (*        (proposer == ("Proposer1" : (proposer1 * proposer2[@tProposerNode])) *)
    (*        && acceptor == ac); *)
    (*      starA (anyA - EAcceptReq (acceptor == ac))), *)
    (*     EAcceptReq (proposer == p && acceptor == ac && va == x), *)
    (*     [| *)
    (*       EAcceptRsp *)
    (*         (proposer == p && acceptor == ac && accepted == p && va == x); *)
    (*     |] )); *)
    (* When is the first accept request recieved, accept it. *)
    (fun ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
      ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( starA (anyA - EAcceptReq (acceptor == ac)),
        EAcceptReq (proposer == p && acceptor == ac && va == x),
        [|
          EAcceptRsp
            (proposer == p && acceptor == ac && accepted == p && va == x);
        |] ));
    (* We can always don't reply. *)
    (fun ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
      ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])]))
      ?l:(x = (true : [%v: tVal]))
    ->
      ( (allA;
         ELostAcceptRsp (proposer == p && acceptor == ac);
         allA),
        EAcceptReq (proposer == p && acceptor == ac && va == x),
        [||] ));
  |]

let eLostAcceptRsp
    ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
    ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])])) =
  (allA, ELostAcceptRsp (proposer == p && acceptor == ac), [||])

let eAcceptRsp =
  [|
    (* When is the first response recieved, also equal it itself send to leaner. (omit the second one) *)
    (fun ?l:(p = (true : [%v: (proposer1 * proposer2[@tProposerNode])]))
      ?l:(ac = (true : [%v: (acceptor1 * acceptor2[@tAcceptorNode])]))
      ?l:(ap = (v == p : [%v: (proposer1 * proposer2[@tProposerNode])]))
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
  starA (anyA - EAcceptRsp true - EPrepareRsp true);
  ELearn (va == y && not (x == y));
  allA
