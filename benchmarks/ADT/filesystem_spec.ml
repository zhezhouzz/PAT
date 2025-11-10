val ( == ) : 'a. 'a -> 'a -> bool
val is_root : Path.t -> bool
val parent : Path.t -> Path.t
val isFile : Byte.t -> bool
val isDir : Byte.t -> bool

let[@axiom] isFileOrDir (b : Byte.t) = isFile b || isDir b
let[@axiom] isDirNotFile (b : Byte.t) = not (isFile b && isDir b)
let[@axiom] parent_is_root (p : Path.t) = iff (p == parent p) (is_root p)
(* Basic Typing *)

val createReq : < path : Path.t ; content : Byte.t > [@@gen]
val initReq : < > [@@gen]
val createResp : < success : bool > [@@obs]
val deleteReq : < path : Path.t > [@@gen]
val deleteResp : < success : bool > [@@obs]
val existsReq : < path : Path.t > [@@gen]
val existsResp : < exists : bool > [@@obs]

(* PATs *)
let initReq = (epsilonA, InitReq true, starA (anyA - InitReq true))

let createReq =
  [|
    (fun ?l:(p = (is_root (parent v) : [%v: Path.t]))
      ?l:(c = (true : [%v: Byte.t]))
    ->
      ( starA (anyA - CreateReq (path == parent p && isDir content)),
        CreateReq (path == p && content == c),
        (CreateResp (success == false);
         allA) ));
    (fun ?l:(p = (not (is_root v) : [%v: Path.t]))
      ?l:(c = (true : [%v: Byte.t]))
    ->
      ( (InitReq true;
         starA (anyA - DeleteReq (path == p));
         CreateReq (path == p);
         CreateResp (success == true);
         starA (anyA - DeleteReq (path == p))),
        CreateReq (path == p && content == c),
        (CreateResp (success == false);
         allA) ));
    (fun ?l:(p = (not (is_root (parent v)) : [%v: Path.t]))
      ?l:(c = (true : [%v: Byte.t]))
    ->
      ( (allA;
         CreateReq (path == parent p && isDir content);
         CreateResp (success == true);
         starA (anyA - DeleteReq (path == parent p))),
        CreateReq (path == p && content == c),
        (CreateResp (success == true);
         allA;
         DeleteReq (path == p);
         allA) ));
    (fun ?l:(p = (is_root (parent v) : [%v: Path.t]))
      ?l:(c = (true : [%v: Byte.t]))
    ->
      ( allA,
        CreateReq (path == p && content == c),
        (CreateResp (success == true);
         allA;
         CreateReq (p == parent path);
         allA;
         DeleteReq (path == p);
         allA) ));
  |]

let createResp =
 fun ?l:(s = (v == true : [%v: bool])) -> (allA, CreateResp (success == s), allA)

let deleteReq =
  [|
    (fun ?l:(p = (is_root v : [%v: Path.t])) ->
      ( allA,
        DeleteReq (path == p),
        (DeleteResp (success == false);
         allA) ));
    (fun ?l:(p = (not (is_root v) : [%v: Path.t])) ->
      ( (allA;
         DeleteReq (path == p);
         starA (anyA - CreateReq (path == p))),
        DeleteReq (path == p),
        (DeleteResp (success == false);
         allA) ));
    (fun ?l:(p = (not (is_root v) : [%v: Path.t])) ->
      ( (InitReq true;
         starA (anyA - DeleteReq (path == p));
         CreateReq (path == p);
         CreateResp (success == true);
         starA (anyA - DeleteReq (path == p))),
        DeleteReq (path == p),
        (DeleteResp (success == true);
         starA (anyA - CreateReq true)) ));
  |]

let deleteResp =
 fun ?l:(s = (v == true : [%v: bool])) -> (allA, DeleteResp (success == s), allA)

let existsReq =
  [|
    (fun ?l:(p = (is_root v : [%v: Path.t])) ->
      ( allA,
        ExistsReq (path == p),
        (ExistsResp (exists == true);
         allA) ));
    (fun ?l:(p = (not (is_root v) : [%v: Path.t])) ->
      ( (allA;
         DeleteReq (path == p);
         starA (anyA - CreateReq (path == p))),
        ExistsReq (path == p),
        (ExistsResp (exists == false);
         allA) ));
    (fun ?l:(p = (not (is_root v) : [%v: Path.t])) ->
      ( (allA;
         CreateReq (path == p);
         CreateResp (success == true);
         starA (anyA - DeleteReq (path == p))),
        ExistsReq (path == p),
        (ExistsResp (exists == true);
         allA) ));
  |]

let existsResp =
 fun ?l:(s = (v == true : [%v: bool])) -> (allA, ExistsResp (exists == s), allA)

(* Global Properties *)

(* delete a none empty directory *)

let[@goal] filesystem (chp : Path.t) =
  allA;
  DeleteReq (path == parent chp);
  DeleteResp (success == true);
  allA;
  ExistsReq (path == chp);
  ExistsResp (exists == true);
  allA
