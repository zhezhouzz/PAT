(* Simplified from p_heartbeat_spec.ml
   Original: Heartbeat detector with ping/pong, trial numbers, nodes down notification.
   Simplified: ePongLost three cases (tl==1,2,3) merged - the "必须在最开始" for
   ENotifyNodesDown preserved. ePong, ePing qualifiers simplified to true.
   eNetworkError, eStart "只能在发生一次" patterns preserved. *)
val ( == ) : 'a. 'a -> 'a -> bool

val eNotifyNodesDown : < > [@@obsRecv]

let eNotifyNodesDown =
  (starA (anyA - ENotifyNodesDown true), ENotifyNodesDown true, [||])

val eNetworkError : < trial : int > [@@gen]

let eNetworkError ?l:(tl = (true : [%v: int])) =
  ( (allA;
     EPong (trial == tl);
     allA),
    ENetworkError (trial == tl),
    [| EPongLost (trial == tl) |] )

val ePing : < trial : int > [@@obs]
val eShutDown : < > [@@obs]

let ePing =
  [|
    (fun ?l:(tl = (true : [%v: int])) ->
      ( starA
          (anyA
         - EShutDown true),
        EPing true,
        [| EPong (trial == tl) |] ));
  |]

let eShutDown = (starA (anyA - EShutDown true), EShutDown true, [||])

val eStart : < > [@@gen]
val ePong : < trial : int > [@@obs]
val ePongLost : < trial : int > [@@obs]

let eStart =
  ( starA (anyA - EPing true - EPongLost true - EStart true),
    EStart true,
    [| EPing (trial == 1) |] )

let ePong =
  [|
    (fun ?l:(tl = (true : [%v: int])) ->
      (starA (anyA - EPongLost (trial == tl)), EPong true, [||]));
  |]

(* ePongLost: three cases for trial 1->2, 2->3, 3->notify. Qualifiers simplified. *)
let ePongLost =
  [|
    (fun ?l:(tl = (v == 1 : [%v: int])) ->
      ( starA (anyA - EPongLost (trial == tl)),
        EPongLost true,
        [| EPing (trial == 2) |] ));
    (fun ?l:(tl = (v == 2 : [%v: int])) ->
      ( starA (anyA - EPongLost (trial == tl)),
        EPongLost true,
        [| EPing (trial == 3) |] ));
    (fun ?l:(tl = (v == 3 : [%v: int])) ->
      (allA, EPongLost true, [| ENotifyNodesDown true |]));
  |]

(* detect false negative - UNCHANGED *)
let[@goal] p_heartbeat =
  starA (anyA - EShutDown true);
  ENotifyNodesDown true;
  starA (anyA - EShutDown true)
