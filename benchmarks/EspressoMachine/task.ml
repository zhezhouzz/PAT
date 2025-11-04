val ( == ) : 'a. 'a -> 'a -> bool
val notWarmedUp : tCoffeeMakerState -> bool
val ready : tCoffeeMakerState -> bool
val noBeansError : tCoffeeMakerState -> bool
val noWaterError : tCoffeeMakerState -> bool
val statId : tCoffeeMakerState -> int

let[@axiom] notWarmedUp (st : tCoffeeMakerState) =
  iff (notWarmedUp st) (statId st == 1)

let[@axiom] ready (st : tCoffeeMakerState) = iff (ready st) (statId st == 2)

let[@axiom] noBeansError (st : tCoffeeMakerState) =
  iff (noBeansError st) (statId st == 3)

let[@axiom] noWaterError (st : tCoffeeMakerState) =
  iff (noWaterError st) (statId st == 4)

let[@axiom] state4 (st : tCoffeeMakerState) =
  statId st == 1 || statId st == 2 || statId st == 3 || statId st == 4

(* event: error message from panel to the user
   1: NotWarmedUp,
   2: Ready,
   3: NoBeansError,
   4: NoWaterError
*)

(** message from env to panel *)

(* event: init *)
val eCoffeeMachineUser : < > [@@gen]

let eCoffeeMachineUser = (allA, ECoffeeMachineUser true, [| EWarmUpReq true |])

(* event: make espresso button pressed *)
val eEspressoButtonPressed : < > [@@gen]

let eEspressoButtonPressed =
  [|
    ( (allA;
       ECoffeeMakerReady true;
       starA (anyA - ECoffeeMakerError true)),
      EEspressoButtonPressed true,
      [| EGrindBeansReq true |] );
  |]

(* (\* event: steamer button turned off *\) *)
(* val eSteamerButtonOff : unit [@@gen] *)

(* (\* event: steamer button turned on *\) *)
(* val eSteamerButtonOn : unit [@@gen] *)

(* (\* event: door opened to empty grounds *\) *)
(* val eOpenGroundsDoor : unit [@@gen] *)

(* (\* event: door closed after emptying grounds *\) *)
(* val eCloseGroundsDoor : unit [@@gen] *)

(* (\* event: reset coffee maker button pressed *\) *)
(* val eResetCoffeeMaker : unit [@@gen] *)

(** message from panel to env *)

val eCoffeeMakerError : < st : tCoffeeMakerState > [@@obsRecv]

let eCoffeeMakerError ?l:(x = (true : [%v: tCoffeeMakerState])) =
  (allA, ECoffeeMakerError (st == x), [||])

(* event: completed brewing and pouring coffee *)
val eCoffeeMakerCompleted : < > [@@obsRecv]

let eCoffeeMakerCompleted = (allA, ECoffeeMakerCompleted true, [||])

(* event: coffee machine is ready *)
val eCoffeeMakerReady : < > [@@obsRecv]

let eCoffeeMakerReady = (allA, ECoffeeMakerReady true, [||])

(** internal messages *)

(* event: warmup request when the coffee maker starts or resets *)
val eWarmUpReq : < > [@@obs]

let eWarmUpReq = (allA, EWarmUpReq true, [| EWarmUpCompleted true |])

(* event: grind beans request before making coffee *)
val eGrindBeansReq : < > [@@obs]

let eGrindBeansReq =
  [|
    (allA, EGrindBeansReq true, [| ENoBeansError true |]);
    (allA, EGrindBeansReq true, [| EGrindBeansCompleted true |]);
  |]

(* event: start brewing coffee *)
val eStartEspressoReq : < > [@@obs]

let eStartEspressoReq =
  [|
    (allA, EStartEspressoReq true, [| ENoWaterError true |]);
    (allA, EStartEspressoReq true, [| EEspressoCompleted true |]);
  |]

(* (\* val start steamer *\) *)
(* val eStartSteamerReq : < > [@@obs] *)

(* (\* event: stop steamer *\) *)
(* val eStopSteamerReq : < > [@@obs] *)

(* Responses from the coffee maker to the controller *)
(* event: completed grinding beans *)
val eGrindBeansCompleted : < > [@@obs]

let eGrindBeansCompleted =
  (allA, EGrindBeansCompleted true, [| EStartEspressoReq true |])

(* event: completed brewing and pouring coffee *)
val eEspressoCompleted : < > [@@obs]

let eEspressoCompleted =
  (allA, EEspressoCompleted true, [| ECoffeeMakerCompleted true |])

(* event: warmed up the machine and read to make coffee *)
val eWarmUpCompleted : < > [@@obs]

let eWarmUpCompleted =
  (allA, EWarmUpCompleted true, [| ECoffeeMakerReady true |])

(* Error messages from the coffee maker to control panel or controller*)
(* event: no water for coffee, refill water! *)
val eNoWaterError : < > [@@obs]

let eNoWaterError =
  (allA, ENoWaterError true, [| ECoffeeMakerError (noWaterError st) |])

(* event: no beans for coffee, refill beans! *)
val eNoBeansError : < > [@@obs]

let eNoBeansError =
  (allA, ENoBeansError true, [| ECoffeeMakerError (noBeansError st) |])

(* no no water error *)
let[@goal] task_EspressoMachine =
  allA;
  ECoffeeMakerError (noWaterError st);
  allA
