(* type tNode = (node1 * node2[@tNode]) *)
(* type tTrial = int *)

val ( == ) : 'a. 'a -> 'a -> bool

(** handled by env *)

val eNotifyNodesDown : unit [@@obsRecv]

let eNotifyNodesDown =
  (starA (anyA - ENotifyNodesDown true), ENotifyNodesDown true, [||])

val eNetworkError : < trial : int > [@@gen]

let eNetworkError ?l:(tl = (true : [%v: int])) =
  ( (allA;
     EPong (trial == tl);
     allA),
    ENetworkError (trial == tl),
    [| EPongLost (trial == tl) |] )

(** Node Machine *)

val ePing : < trial : int > [@@obs]
val eShutDown : unit [@@obs]

let ePing =
  [|
    (fun ?l:(tl = (true : [%v: int])) ->
      ( starA
          (anyA
         (* - EPing ( trial == tl) *)
         - EShutDown true),
        EPing (trial == tl),
        [| EPong (trial == tl) |] ));
  |]

let eShutDown = (starA (anyA - EShutDown true), EShutDown true, [||])

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
      (allA, EPongLost (trial == tl), [| ENotifyNodesDown true |]));
  |]

(* detect false negative *)
let[@goal] task_HeartBeat =
  starA (anyA - EShutDown true);
  ENotifyNodesDown true;
  starA (anyA - EShutDown true)
