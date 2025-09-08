val ( == ) : 'a. 'a -> 'a -> bool
val is_root : Path.t -> bool
val parent : Path.t -> Path.t

let[@axiom] parent_is_root (p : Path.t) = iff (p == parent p) (is_root p)
(* Basic Typing *)

val createDirReq : < path : Path.t > [@@gen]
val initReq : < > [@@gen]
val createDirResp : < success : bool > [@@obs]
val deletePathReq : < path : Path.t > [@@gen]
val deletePathResp : < success : bool > [@@obs]

(* PATs *)
let initReq = (epsilonA, InitReq true, starA (anyA - InitReq true))

let createDirReq =
  [|
    (fun ?l:(p = (is_root (parent v) : [%v: Path.t])) ->
      ( allA,
        CreateDirReq (path == p),
        (CreateDirResp (success == true);
         allA;
         DeletePathReq (path == p);
         allA) ));
    (fun ?l:(p = (not (is_root (parent v)) : [%v: Path.t])) ->
      ( (allA;
         CreateDirReq (path == parent p);
         CreateDirResp (success == true);
         starA (anyA - DeletePathReq (path == parent p))),
        CreateDirReq (path == p),
        (CreateDirResp (success == true);
         allA;
         DeletePathReq (path == p);
         allA) ));
  |]

let createDirResp =
 fun ?l:(s = (v == true : [%v: bool])) ->
  (allA, CreateDirResp (success == s), allA)

let deletePathReq =
 fun ?l:(p = (not (is_root v) : [%v: Path.t])) ->
  ( (InitReq true;
     allA;
     CreateDirReq (path == p);
     CreateDirResp (success == true);
     starA (anyA - DeletePathReq (path == p))),
    DeletePathReq (path == p),
    (DeletePathResp (success == true);
     allA) )

let deletePathResp =
 fun ?l:(s = (v == true : [%v: bool])) ->
  (allA, DeletePathResp (success == s), allA)

(* Global Properties *)

let[@goal] deleteNoneEmptyDir (chp : Path.t) =
  allA;
  CreateDirReq (path == chp);
  CreateDirResp (success == true);
  starA (anyA - DeletePathReq (path == chp));
  DeletePathReq (path == parent chp);
  DeletePathResp (success == true);
  allA
