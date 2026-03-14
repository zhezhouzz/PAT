val ( == ) : 'a. 'a -> 'a -> bool
val ( >= ) : int -> int -> bool
val ( + ) : int -> int -> int
val initSet : < > [@@gen]
val insert : < elem : int > [@@gen]
val deleteReq : < elem : int > [@@gen]
val deleteResp : < res : bool > [@@obs]
val isEmptyReq : < > [@@gen]
val isEmptyResp : < isEmpty : bool > [@@obs]
val size : < sz : int > [@@gen]

let initSet =
  ( allA,
    InitSet true,
    (Size (sz == 0);
     allA) )

let insert (i : int) ?l:(x = (true : [%v: int])) =
  ( (starA (anyA - Insert (elem >= x));
     Size (sz == i)),
    Insert (elem == x),
    (Size (sz == i + 1);
     allA;
     DeleteReq (elem == x);
     allA) )

let deleteReq =
  [|
    (fun (i : int) ?l:(x = (true : [%v: int])) ->
      ( (starA (anyA - DeleteReq (elem == x));
         Insert (elem == x);
         starA (anyA - DeleteReq (elem == x));
         Size (sz == i + 1 && i >= 0)),
        DeleteReq (elem == x),
        (DeleteResp true;
         Size (sz == i);
         allA) ));
    (* (fun (i : int) ?l:(x = (true : [%v: int])) ->
      ( (starA (anyA - Insert (elem == x));
         Size (sz == i)),
        DeleteReq (elem == x),
        (DeleteResp (res == false);
         Size (sz == i);
         allA) )); *)
  |]

let deleteResp = (allA, DeleteResp true, allA)

let isEmptyReq =
  ( allA,
    IsEmptyReq true,
    (IsEmptyResp true;
     allA) )

let size =
  [|
    (fun ?l:(i = (true : [%v: int])) -> (allA, Size (sz == i), allA));
    (* (fun ?l:(i = (true : [%v: int])) ->
      ( (allA;
         InitSet true),
        Size (sz == i),
        allA ));
    (fun ?l:(i = (true : [%v: int])) ->
      ( (allA;
         Insert true),
        Size (sz == i),
        allA ));
    (fun ?l:(i = (true : [%v: int])) ->
      ( (allA;
         DeleteResp true),
        Size (sz == i),
        allA )); *)
  |]

let isEmptyResp = (allA, IsEmptyResp true, allA)

let[@goal] set (y : int) =
  InitSet true;
  allA;
  Insert (elem == y);
  allA;
  DeleteReq (not (elem == y));
  allA
