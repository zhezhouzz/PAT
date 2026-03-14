val ( == ) : 'a. 'a -> 'a -> bool
val ( > ) : int -> int -> bool
val ( + ) : int -> int -> int
val ( - ) : int -> int -> int
val isAddr : int -> bool

let[@axiom] isAddr (x : int) = iff (isAddr x) (2 > x && x > -1)

(* Basic Typing *)

val push : < low : bool ; lelem : int ; relem : int > [@@gen]
val pop : < > [@@gen]
val load : < > [@@gen]
val store : < > [@@gen]
val add : < > [@@gen]
val enniReq : < > [@@gen]
val stackDepth : < depth : int > [@@obs]
val enniResp : < enni : bool > [@@obs]

(* PATs *)

let push =
  [|
    (fun ?l:(lv = (true : [%v: bool]))
      ?l:(x = (true : [%v: int]))
      ?l:(y = (lv == (v == x) : [%v: int]))
    ->
      ( starA (anyA - StackDepth true),
        Push
          (low == lv && lelem == x && relem == y && isAddr lelem && isAddr relem),
        (StackDepth (depth == 1);
         allA) ));
    (fun (d : int)
      ?l:(lv = (true : [%v: bool]))
      ?l:(x = (true : [%v: int]))
      ?l:(y = (lv == (v == x) : [%v: int]))
    ->
      ( (allA;
         StackDepth (depth == d)),
        Push
          (low == lv && lelem == x && relem == y && isAddr lelem && isAddr relem),
        (StackDepth (depth == d + 1);
         allA) ));
  |]

let pop (d : int) =
  ( (allA;
     StackDepth (depth == d && depth > 0)),
    Pop true,
    (StackDepth (depth == d - 1);
     allA) )

let load =
  [|
    (fun (d : int) ->
      ( (allA;
         Store true;
         allA;
         Push true;
         StackDepth (depth == d && depth > 0)),
        Load true,
        (StackDepth (depth == d);
         allA;
         Store true;
         allA) ));
  |]

let store =
  [|
    (fun (d : int) ->
      ( (allA;
         Push true;
         StackDepth (depth == d && depth > 1)),
        Store true,
        (StackDepth (depth == d - 2);
         allA) ));
    (fun (d : int) ->
      ( (allA;
         StackDepth (depth == d && depth > 1)),
        Store true,
        (StackDepth (depth == d - 2);
         allA) ));
  |]

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
