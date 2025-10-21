val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val ( <= ) : int -> int -> bool
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

val const : < > [@@gen]
val mkVar : < bid : int > [@@gen]
val mkAbs : < sid : int ; ty : stlcTy > [@@gen]
val endAbs : < sid : int > [@@gen]
val mkApp : < sid : int > [@@gen]
val endAppL : < sid : int > [@@gen]
val endAppR : < sid : int > [@@gen]
val putDepth : < d : int > [@@gen]
val asyncGetDepth : < > [@@gen]
val getDepth : < d : int > [@@obs]
val putTy : < ty : stlcTy > [@@gen]

let const =
  ( (allA;
     anyA - PutTy true),
    Const true,
    (PutTy (is_int_ty ty);
     allA) )

let mkApp =
 fun (tp : stlcTy) (i : int) ->
  ( starA (anyA - ClosureId (sid >= i)),
    MkApp true,
    (ClosureId (sid == i);
     starA (anyA - EndAppL (sid <= i) - EndAppR (sid <= i) - EndAbs (sid <= i));
     EndAppL (sid == i && ty == tp);
     starA (anyA - EndAppL (sid <= i) - EndAppR (sid <= i) - EndAbs (sid <= i));
     EndAppR (sid == i && fstTy (tp, ty));
     PutTy (sndTy (tp, ty));
     allA) )

let mkVar =
  [|
    (fun (tp : stlcTy) ?l:(i = (true : [%v: int])) ->
      ( (allA;
         MkAbs (ty == tp);
         starA (anyA - EndAbs true)),
        MkVar (bid == i && bid == 0),
        (PutTy (ty == tp);
         allA) ));
    (fun (tp : stlcTy) ?l:(i = (true : [%v: int])) ->
      ( (allA;
         MkAbs (ty == tp);
         starA (anyA - EndAbs true);
         MkAbs true;
         starA (anyA - EndAbs true);
         MkVar (bid == i && bid == 1)),
        PutTy (ty == tp),
        allA ));
  |]

let mkAbs (tp : stlcTy) (i : int) ?l:(x = (true : [%v: stlcTy])) =
  ( starA (anyA - ClosureId (sid >= i)),
    MkAbs (ty == x && fstTy (tp, ty)),
    (ClosureId (sid == i);
     starA (anyA - EndAppL (sid <= i) - EndAppR (sid <= i) - EndAbs (sid <= i));
     MkVar true;
     starA (anyA - EndAppL (sid <= i) - EndAppR (sid <= i) - EndAbs (sid <= i));
     EndAbs (sid == i && sndTy (tp, ty));
     PutTy (ty == tp);
     allA) )

let endAbs ?l:(i = (true : [%v: int])) ?l:(x = (true : [%v: stlcTy])) =
  ( (allA;
     PutTy (ty == x)),
    EndAbs (sid == i && ty == x),
    allA )

let endAppL ?l:(i = (true : [%v: int])) ?l:(tp = (true : [%v: stlcTy])) =
  ( (allA;
     PutTy (ty == tp)),
    EndAppL (sid == i && ty == tp),
    allA )

let endAppR ?l:(i = (true : [%v: int])) ?l:(tp = (true : [%v: stlcTy])) =
  ( (allA;
     PutTy (ty == tp)),
    EndAppR (sid == i && ty == tp),
    allA )

let ascribe =
  [| (fun ?l:(x = (true : [%v: stlcTy])) -> (allA, PutTy (ty == x), allA)) |]

let closureId =
  [| (fun ?l:(i = (true : [%v: int])) -> (allA, ClosureId (sid == i), allA)) |]

let[@goal] stlc =
  allA;
  MkApp true;
  allA;
  MkApp true;
  allA;
  EvalReq true;
  EvalResp (res == true)
