val ( == ) : 'a. 'a -> 'a -> bool
val ( > ) : int -> int -> bool
val ( + ) : int -> int -> int
val ( - ) : int -> int -> int

(* Basic Typing *)

val pushPublic : < elem : int > [@@gen]
val pushPrivate : < lelem : int ; relem : int > [@@gen]
val pop : < > [@@gen]
val load : < > [@@gen]
val store : < > [@@gen]
val add : < > [@@gen]
val enniReq : < > [@@gen]
val stackDepth : < depth : int > [@@obs]
val enniResp : < enni : bool > [@@obs]

(* PATs *)
let pushPublic =
  [|
    (fun ?l:(x = (true : [%v: int])) ->
      ( starA (anyA - StackDepth true),
        PushPublic (elem == x && 2 > elem && elem > -1),
        (StackDepth (depth == 1);
         allA) ));
    (fun (d : int) ?l:(x = (true : [%v: int])) ->
      ( (allA;
         StackDepth (depth == d)),
        PushPublic (elem == x && 2 > elem && elem > -1),
        (StackDepth (depth == d + 1);
         allA) ));
  |]

let pushPrivate =
  [|
    (fun ?l:(x = (true : [%v: int])) ?l:(y = (true : [%v: int])) ->
      ( starA (anyA - StackDepth true),
        PushPrivate
          (lelem == x && relem == y
          && (not (lelem == relem))
          && 2 > lelem && 2 > relem && lelem > -1 && relem > -1),
        (StackDepth (depth == 1);
         allA) ));
    (fun (d : int) ?l:(x = (true : [%v: int])) ?l:(y = (true : [%v: int])) ->
      ( (allA;
         StackDepth (depth == d)),
        PushPrivate
          (lelem == x && relem == y
          && (not (lelem == relem))
          && 2 > lelem && 2 > relem && lelem > -1 && relem > -1),
        (StackDepth (depth == d + 1);
         allA) ));
  |]

let pop (d : int) =
  ( (allA;
     StackDepth (depth == d && depth > 0)),
    Pop true,
    (StackDepth (depth == d - 1);
     allA) )

let load (d : int) =
  ( (allA;
     Store true;
     allA;
     PushPrivate (lelem == 0 && relem == 1);
     StackDepth (depth == d && depth > 0)),
    Load true,
    (StackDepth (depth == d);
     Store true;
     allA;
     allA) )

let store (d : int) =
  ( (allA;
     StackDepth (depth == d && depth > 1)),
    Store true,
    (StackDepth (depth == d - 2);
     allA) )

let add (d : int) =
  ( (allA;
     StackDepth (depth == d && depth > 1)),
    Add true,
    (StackDepth (depth == d - 1);
     allA) )

let enniReq =
  ( allA,
    EnniReq true,
    (EnniResp true;
     allA) )

let enniResp ?l:(x = (true : [%v: bool])) = (allA, EnniResp (enni == x), allA)

let stackDepth ?l:(d = (true : [%v: int])) =
  (allA, StackDepth (depth == d), allA)

let[@goal] ifc_store =
  allA;
  Store true;
  allA;
  EnniResp (enni == true)

let[@goal] ifc_add =
  allA;
  Add true;
  allA;
  EnniResp (enni == true)

let[@goal] ifc_load =
  allA;
  Load true;
  allA;
  EnniResp (enni == true)
