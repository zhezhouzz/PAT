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

val mkCon : < > [@@gen]
val mkVar : < bid : int > [@@gen]
val mkAbs : < ty : stlcTy > [@@gen]
val closeAbs : < sid : int ; ty : stlcTy > [@@gen]
val mkApp : < > [@@gen]
val closeAppL : < sid : int ; ty : stlcTy > [@@gen]
val closeAppR : < sid : int ; ty : stlcTy > [@@gen]
val evalReq : < > [@@gen]
val evalResp : < res : bool > [@@obs]
val curTy : < ty : stlcTy > [@@obs]
val closureId : < sid : int > [@@obs]

(* PATs *)

let mkVar =
  [|
    (fun (tp : stlcTy) ?l:(i = (true : [%v: int])) ->
      ( (allA;
         MkAbs (ty == tp);
         starA (anyA - CloseAbs true)),
        MkVar (bid == i && bid == 0),
        (CurTy (ty == tp);
         allA) ));
    (fun (tp : stlcTy) ?l:(i = (true : [%v: int])) ->
      ( (allA;
         MkAbs (ty == tp);
         starA (anyA - CloseAbs true);
         MkAbs true;
         starA (anyA - CloseAbs true);
         MkVar (bid == i && bid == 1)),
        CurTy (ty == tp),
        allA ));
  |]

let mkCon =
  ( allA,
    MkCon true,
    (CurTy (is_int_ty ty);
     allA) )

let mkAbs (tp : stlcTy) (i : int) ?l:(x = (true : [%v: stlcTy])) =
  ( starA (anyA - ClosureId (sid >= i)),
    MkAbs (ty == x && fstTy (tp, ty)),
    (ClosureId (sid == i);
     starA
       (anyA - CloseAppL (sid <= i) - CloseAppR (sid <= i) - CloseAbs (sid <= i));
     MkVar true;
     starA
       (anyA - CloseAppL (sid <= i) - CloseAppR (sid <= i) - CloseAbs (sid <= i));
     CloseAbs (sid == i && sndTy (tp, ty));
     CurTy (ty == tp);
     allA) )

let closeAbs ?l:(i = (true : [%v: int])) ?l:(x = (true : [%v: stlcTy])) =
  ( (allA;
     CurTy (ty == x)),
    CloseAbs (sid == i && ty == x),
    allA )

let mkApp =
  [|
    (fun (tp : stlcTy) (i : int) ->
      ( starA (anyA - ClosureId (sid >= i)),
        MkApp true,
        (ClosureId (sid == i);
         starA
           (anyA
           - CloseAppL (sid <= i)
           - CloseAppR (sid <= i)
           - CloseAbs (sid <= i));
         CloseAppL (sid == i && ty == tp);
         starA
           (anyA
           - CloseAppL (sid <= i)
           - CloseAppR (sid <= i)
           - CloseAbs (sid <= i));
         CloseAppR (sid == i && fstTy (tp, ty));
         CurTy (sndTy (tp, ty));
         allA) ));
  |]

let closeAppL ?l:(i = (true : [%v: int])) ?l:(tp = (true : [%v: stlcTy])) =
  ( (allA;
     CurTy (ty == tp)),
    CloseAppL (sid == i && ty == tp),
    allA )

let closeAppR ?l:(i = (true : [%v: int])) ?l:(tp = (true : [%v: stlcTy])) =
  ( (allA;
     CurTy (ty == tp)),
    CloseAppR (sid == i && ty == tp),
    allA )

let evalReq =
  ( allA,
    EvalReq true,
    (EvalResp true;
     allA) )

let evalResp ?l:(x = (true : [%v: bool])) = (allA, EvalResp (res == x), allA)

let curTy =
  [| (fun ?l:(x = (true : [%v: stlcTy])) -> (allA, CurTy (ty == x), allA)) |]

let closureId =
  [| (fun ?l:(i = (true : [%v: int])) -> (allA, ClosureId (sid == i), allA)) |]

(* Global Properties *)

let[@goal] stlc =
  allA;
  MkApp true;
  allA;
  MkApp true;
  allA;
  EvalReq true;
  EvalResp (res == true)
