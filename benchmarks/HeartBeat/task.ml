(* type tNode = (node1 * node2[@tNode]) *)
(* type tTrial = int *)

val ( == ) : 'a -> 'a -> bool

(** handled by env *)

val eNotifyNodesDone : unit [@@obsRecv]

let eNotifyNodesDone =
  (starA (anyA - ENotifyNodesDone true), ENotifyNodesDone true, [||])

val eNetworkError : < trial : int > [@@gen]

let eNetworkError ?l:(tl = (true : [%v: int])) =
  ( (starA
       (anyA
       - ENetworkError (trial == tl)
       - EPongLost (trial == tl)
       - EPong (trial == tl));
     EPong (trial == tl);
     starA
       (anyA
       - ENetworkError (trial == tl)
       - EPongLost (trial == tl)
       - EPong (trial == tl))),
    ENetworkError (trial == tl),
    [| EPongLost (trial == tl) |] )

(** Node Machine *)

val ePing : < trial : int > [@@obs]
val eShutDone : unit [@@obs]

let ePing =
  [|
    (fun ?l:(tl = (true : [%v: int])) ->
      ( starA
          (anyA
         (* - EPing ( trial == tl) *)
         - EShutDone true
          - EPongLost (trial == tl)
          - EPong (trial == tl)),
        EPing (trial == tl),
        [| EPong (trial == tl) |] ));
  |]

let eShutDone = (starA (anyA - EShutDone true), EShutDone true, [||])

(** Detector Machine *)

val eStart : unit [@@gen]
val ePong : < trial : int > [@@obs]
val ePongLost : < trial : int > [@@obs]

let eStart =
  ( starA (anyA - EPing true - EPongLost true - EStart true),
    EStart true,
    [| EPing (trial == 1) |] )

let ePong =
  [|
    (* Trail1: all pongs are received, done *)
    (fun ?l:(tl = (true : [%v: int])) ->
      (starA (anyA - EPongLost (trial == tl)), EPong (trial == tl), [||]));
  |]

let ePongLost =
  [|
    (fun ?l:(tl = (v == 1 : [%v: int])) ->
      ( starA (anyA - EPongLost (trial == tl)),
        EPongLost (trial == tl),
        [| EPing (trial == 2) |] ));
    (fun ?l:(tl = (v == 2 : [%v: int])) ->
      ( starA (anyA - EPongLost (trial == tl)),
        EPongLost (trial == tl),
        [| EPing (trial == 3) |] ));
    (fun ?l:(tl = (v == 3 : [%v: int])) ->
      (allA, EPongLost (trial == tl), [| ENotifyNodesDone true |]));
  |]

let[@goal] detectFalseNegative =
  not
    (starA (anyA - EShutDone true);
     ENotifyNodesDone true;
     starA (anyA - EShutDone true))
