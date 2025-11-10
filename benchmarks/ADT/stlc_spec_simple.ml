val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val ( <= ) : int -> int -> bool
val ( + ) : int -> int -> int
val is_int_ty : stlcTy -> bool
val fstTy : stlcTy * stlcTy -> bool
val sndTy : stlcTy * stlcTy -> bool

let[@axiom] fst_not_eq_self (a : stlcTy) (b : stlcTy) =
  implies (fstTy (a, b)) (not (a == b))

let[@axiom] snd_not_eq_self (a : stlcTy) (b : stlcTy) =
  implies (sndTy (a, b)) (not (a == b))

let[@axiom] fst_implies_not_int (a : stlcTy) (b : stlcTy) =
  implies (fstTy (a, b)) (not (is_int_ty a))

let[@axiom] snd_implies_not_int (a : stlcTy) (b : stlcTy) =
  implies (sndTy (a, b)) (not (is_int_ty a))

let[@axiom] same_implies_fst_same (a : stlcTy) (b : stlcTy) (c : stlcTy) =
  implies (fstTy (a, b) && fstTy (a, c)) (b == c)

let[@axiom] same_implies_snd_same (a : stlcTy) (b : stlcTy) (c : stlcTy) =
  implies (sndTy (a, b) && sndTy (a, c)) (b == c)

let[@axiom] fst_snd_same_implies_same (a : stlcTy) (b : stlcTy) (c : stlcTy)
    (d : stlcTy) =
  implies (fstTy (a, c) && sndTy (a, d) && fstTy (b, c) && sndTy (b, d)) (a == b)

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
         TyClose (sid == i && is_int_ty ty);
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) ));
    (fun ?l:(i = (true : [%v: int])) ->
      ( starA (anyA - TyOpen (sid == i) - TyClose (sid == i)),
        TyOpen (sid == i),
        (Var true;
         TyClose (sid == i);
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) ));
    (* (fun ?l:(i = (true : [%v: int])) ->
      ( starA (anyA - TyOpen (sid == i) - TyClose (sid == i)),
        TyOpen (sid == i),
        (Abs true;
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         TyClose (sid == i && not (is_int_ty ty));
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) )); *)
    (fun ?l:(i = (true : [%v: int])) ->
      ( (starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         App true),
        TyOpen (sid == i),
        (Var true;
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         TyClose (sid == i && not (is_int_ty ty));
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) ));
    (fun ?l:(i = (true : [%v: int])) ->
      ( (starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         App true),
        TyOpen (sid == i),
        (Abs true;
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         TyClose (sid == i && not (is_int_ty ty));
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) ));
    (fun ?l:(i = (true : [%v: int])) ->
      ( (starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         AppL true),
        TyOpen (sid == i),
        (Abs true;
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         TyClose (sid == i && not (is_int_ty ty));
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) ));
    (* (fun ?l:(i = (true : [%v: int])) ->
      ( starA (anyA - TyOpen (sid == i) - TyClose (sid == i)),
        TyOpen (sid == i),
        (App true;
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i));
         TyClose (sid == i);
         starA (anyA - TyOpen (sid == i) - TyClose (sid == i))) )); *)
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
      (allA, TyClose (sid == i && ty == tp), allA));
  |]

let depth = fun ?l:(d1 = (true : [%v: int])) -> (allA, Depth (d == d1), allA)

let const =
  [|
    (fun (i : int) ->
      ( (allA;
         TyOpen (sid == i)),
        Const true,
        (TyClose (sid == i && is_int_ty ty);
         allA) ));
  |]

let app =
  [|
    (fun (tp : stlcTy) (i : int) (j : int) (w : int) ->
      ( (allA;
         TyOpen (sid == i)),
        App true,
        (TyOpen (sid == j);
         Var true;
         TyClose (sid == j && ty == tp);
         AppL true;
         TyOpen (sid == w);
         Const true;
         TyClose (sid == w && fstTy (tp, ty));
         AppR true;
         TyClose (sid == i && sndTy (tp, ty));
         allA) ));
    (fun (tp : stlcTy) (i : int) (j : int) (w : int) ->
      ( (allA;
         TyOpen (sid == i)),
        App true,
        (TyOpen (sid == j);
         allA;
         TyClose (sid == j && ty == tp);
         AppL true;
         TyOpen (sid == w);
         allA;
         TyClose (sid == w && fstTy (tp, ty));
         AppR true;
         TyClose (sid == i && sndTy (tp, ty));
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
        Abs (ty == x && fstTy (tp, ty)),
        (Depth (d == d1 + 1);
         TyOpen (sid == j);
         allA;
         TyClose (sid == j && sndTy (tp, ty));
         EndAbs true;
         Depth (d == d1);
         TyClose (sid == i && ty == tp);
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
        (TyClose (sid == i && ty == tp);
         allA) ));
    (fun (i : int) (d1 : int) (tp : stlcTy) ?l:(idx = (true : [%v: int])) ->
      ( (allA;
         Abs (ty == tp);
         Depth (d == d1);
         starA (anyA - EndAbs true);
         Depth (d == d1 + idx);
         TyOpen (sid == i)),
        Var (bid == idx),
        (TyClose (sid == i && ty == tp);
         allA) ));
  |]

let[@goal] stlc1 (i : int) =
  Depth (d == 0);
  TyOpen (sid == i);
  allA;
  Var true;
  allA;
  TyClose (sid == i && is_int_ty ty)
