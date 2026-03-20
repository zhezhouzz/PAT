val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val ( <= ) : int -> int -> bool
val ( + ) : int -> int -> int

(* Basic Typing *)

val tyOpen : < sid : int > [@@gen]
val tyClose : < sid : int ; ty : stlcTy > [@@gen]
val depth : < d : int > [@@gen]
val app : < > [@@gen]
val appL : < > [@@gen]
val appR : < > [@@gen]
val const : < > [@@gen]
val var : < bid : int > [@@gen]
val abs : < ty : stlcTy > [@@gen]
val endAbs : < > [@@gen]

let tyOpen =
  [|
    (fun ?l:(i = (true : [%v: int])) ->
      ( starA (anyA - TyOpen (sid == i) - TyClose (sid == i)),
        TyOpen (sid == i),
        (Const true;
         TyClose (sid == i);
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) ));
    (fun ?l:(i = (true : [%v: int])) ->
      ( starA (anyA - TyOpen (sid == i) - TyClose (sid == i)),
        TyOpen (sid == i),
        (Var true;
         TyClose (sid == i);
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) ));
    (fun ?l:(i = (true : [%v: int])) ->
      ( (starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         App true),
        TyOpen (sid == i),
        (Var true;
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         TyClose (sid == i);
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) ));
    (fun ?l:(i = (true : [%v: int])) ->
      ( (starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         App true),
        TyOpen (sid == i),
        (Abs true;
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         TyClose (sid == i);
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) ));
    (fun ?l:(i = (true : [%v: int])) ->
      ( (starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         AppL true),
        TyOpen (sid == i),
        (Abs true;
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         TyClose (sid == i);
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) ));
    (fun ?l:(i = (true : [%v: int])) ->
      ( (starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         Depth true),
        TyOpen (sid == i),
        (App true;
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         TyClose (sid == i);
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) ));
  |]

let tyClose =
  [|
    (fun ?l:(i = (true : [%v: int])) ?l:(tp = (true : [%v: stlcTy])) ->
      (allA, TyClose (sid == i), allA));
  |]

let depth = fun ?l:(d1 = (true : [%v: int])) -> (allA, Depth (d == d1), allA)

let const =
  [|
    (fun (i : int) ->
      ( (allA;
         TyOpen (sid == i)),
        Const true,
        (TyClose (sid == i);
         allA) ));
  |]

let app =
  [|
    (fun (tp : stlcTy) (i : int) (j : int) (w : int) ->
      ( (allA;
         TyOpen (sid == i)),
        App true,
        (TyOpen (sid == j);
         allA;
         TyClose (sid == j);
         AppL true;
         TyOpen (sid == w);
         allA;
         TyClose (sid == w);
         AppR true;
         TyClose (sid == i);
         allA) ));
  |]

let appL = (allA, AppL true, allA)
let appR = (allA, AppR true, allA)

let abs =
  [|
    (fun (d1 : int)
      (tp : stlcTy)
      (i : int)
      (j : int)
      ?l:(x = (true : [%v: stlcTy]))
    ->
      ( (allA;
         Depth (d == d1);
         starA (anyA - Depth true);
         TyOpen (sid == i)),
        Abs (ty == x),
        (Depth (d == d1 + 1);
         TyOpen (sid == j);
         allA;
         TyClose (sid == j);
         EndAbs true;
         Depth (d == d1);
         TyClose (sid == i);
         allA) ));
  |]

let endAbs = (allA, EndAbs true, allA)

let var =
  [|
    (fun (i : int) (d1 : int) (tp : stlcTy) ?l:(idx = (true : [%v: int])) ->
      ( (allA;
         Abs (ty == tp);
         Depth (d == d1);
         starA (anyA - Depth true);
         TyOpen (sid == i)),
        Var (bid == 0),
        (TyClose (sid == i);
         allA) ));
    (fun (i : int) (d1 : int) (tp : stlcTy) ?l:(idx = (true : [%v: int])) ->
      ( (allA;
         Abs (ty == tp);
         Depth (d == d1);
         starA (anyA - EndAbs true);
         Depth (d == d1 + idx);
         TyOpen (sid == i)),
        Var (bid == idx),
        (TyClose (sid == i);
         allA) ));
  |]

let[@goal] debruijn1 (i : int) =
  Depth (d == 0);
  TyOpen (sid == i);
  allA;
  Var true;
  allA;
  TyClose (sid == i)
