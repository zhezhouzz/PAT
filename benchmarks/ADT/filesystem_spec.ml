val ( == ) : 'a. 'a -> 'a -> bool
val is_root : Path.t -> bool
val parent : Path.t -> Path.t

let[@axiom] parent_is_root (p : Path.t) = iff (p == parent p) (is_root p)
(* Basic Typing *)

val createDirReq : < path : Path.t > [@@gen]
val initReq : < > [@@gen]
val createDirResp : < success : bool > [@@obs]
val deleteDirReq : < path : Path.t > [@@gen]
val deleteDirResp : < success : bool > [@@obs]

(* PATs *)
let initReq = (epsilonA, InitReq true, starA (anyA - InitReq true))

let createDirReq =
  [|
    (fun ?l:(p = (is_root (parent v) : [%v: Path.t])) ->
      ( allA,
        CreateDirReq (path == p),
        (CreateDirResp (success == true);
         allA;
         DeleteDirReq (path == p);
         allA) ));
    (fun ?l:(p = (not (is_root (parent v)) : [%v: Path.t])) ->
      ( (allA;
         CreateDirReq (path == parent p);
         CreateDirResp (success == true);
         starA (anyA - DeleteDirReq (path == parent p))),
        CreateDirReq (path == p),
        (CreateDirResp (success == true);
         allA;
         DeleteDirReq (path == p);
         allA) ));
  |]

let createDirResp =
 fun ?l:(s = (v == true : [%v: bool])) ->
  (allA, CreateDirResp (success == s), allA)

let deleteDirReq =
 fun ?l:(p = (not (is_root v) : [%v: Path.t])) ->
  ( (allA;
     CreateDirReq (path == p);
     CreateDirResp (success == true);
     starA (anyA - DeleteDirReq (path == p))),
    DeleteDirReq (path == p),
    (DeleteDirResp (success == true);
     allA) )

let deleteDirResp =
 fun ?l:(s = (v == true : [%v: bool])) ->
  (allA, DeleteDirResp (success == s), allA)

(* Global Properties *)

(* let[@goal] deleteNoneEmptyDir (chp : Path.t) =
  allA;
  CreateDirReq (path == parent chp);
  CreateDirResp (success == true);
  starA (anyA - DeleteDirReq (path == chp));
  CreateDirReq (path == chp);
  CreateDirResp (success == true);
  starA (anyA - DeleteDirReq (path == chp));
  DeleteDirReq (path == parent chp);
  DeleteDirResp (success == true);
  allA *)

let[@goal] deleteNoneEmptyDir (chp : Path.t) =
  allA;
  CreateDirReq (path == chp);
  CreateDirResp (success == true);
  starA (anyA - DeleteDirReq (path == chp));
  DeleteDirReq (path == parent chp);
  DeleteDirResp (success == true);
  allA
