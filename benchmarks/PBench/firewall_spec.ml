(* type tNode = tNode *)

val ( == ) : 'a. 'a -> 'a -> bool
val eStart : < node : tNode > [@@gen]
val eInternalReq : < node : tNode > [@@obs]
val eForwardReq : < node : tNode > [@@obs]
val eExternalReq : < node : tNode > [@@obs]
val eExternalRsp : < node : tNode ; stat : bool > [@@obs]

let eStart =
  [|
    (fun ?l:(n = (true : [%v: tNode])) ->
      (allA, EStart (node == n), [| EInternalReq (node == n) |]));
    (* (fun ?l:(n = (true : [%v: tNode])) -> *)
    (*   (allA, EStart (node == n), [||])); *)
  |]

let eInternalReq ?l:(n = (true : [%v: tNode])) =
  ( starA
      (anyA
      - EExternalReq (node == n)
      - EInternalReq (node == n)
      - EForwardReq (node == n)),
    EInternalReq (node == n),
    [| EForwardReq (node == n) |] )

let eForwardReq =
  [|
    (fun ?l:(n = (true : [%v: tNode])) ->
      ( starA (anyA - EForwardReq (node == n)),
        EForwardReq (node == n),
        [| EExternalReq (node == n) |] ));
    (* (fun ?l:(n = (true : [%v: tNode])) -> *)
    (*   (starA (anyA - EForwardReq (node == n)), EForwardReq (node == n), [||])); *)
  |]

let eExternalReq =
  [|
    (fun ?l:(n = (true : [%v: tNode])) ->
      ( (allA;
         EInternalReq (node == n);
         starA (anyA - EInternalReq true)),
        EExternalReq (node == n),
        [| EExternalRsp (node == n && stat) |] ));
    (* (fun ?l:(n = (true : [%v: tNode])) -> *)
    (*   ( starA (anyA - EInternalReq (node == n)), *)
    (*     EExternalReq (node == n), *)
    (*     [| EExternalRsp (node == n && not stat) |] )); *)
    (fun ?l:(n = (true : [%v: tNode])) ->
      ( (allA;
         EInternalReq (not (node == n));
         starA (anyA - EInternalReq true)),
        EExternalReq (node == n),
        [| EExternalRsp (node == n && not stat) |] ));
  |]

let eExternalRsp ?l:(n = (true : [%v: tNode])) ?l:(st = (true : [%v: bool])) =
  (allA, EExternalRsp (node == n && stat == st), [||])

(* allow all session from internal node *)

let[@goal] task_Firewall (n : tNode) =
  starA (anyA - EExternalReq (node == n) - EForwardReq (node == n));
  EInternalReq (node == n);
  (* allA; *)
  (* EExternalReq (node == n); *)
  allA;
  EExternalRsp (node == n && not stat);
  allA
