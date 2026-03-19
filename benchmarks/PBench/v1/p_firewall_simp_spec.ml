(* Simplified from p_firewall_spec.ml
   Original: Internal requests must not have ExternalReq/InternalReq/ForwardReq on same node before.
   Simplified: Event qualifiers in eInternalReq, eForwardReq, eExternalReq simplified to `true`
   where the node linking is preserved by structure. eExternalRsp simplified to true.
   "只能发生一次" (starA anyA - X) patterns preserved. *)
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
  |]

let eInternalReq ?l:(n = (true : [%v: tNode])) =
  ( starA
      (anyA
      - EExternalReq (node == n)
      - EInternalReq (node == n)
      - EForwardReq (node == n)),
    EInternalReq true,
    [| EForwardReq (node == n) |] )

let eForwardReq =
  [|
    (fun ?l:(n = (true : [%v: tNode])) ->
      ( starA (anyA - EForwardReq (node == n)),
        EForwardReq true,
        [| EExternalReq (node == n) |] ));
  |]

let eExternalReq =
  [|
    (fun ?l:(n = (true : [%v: tNode])) ->
      ( (allA;
         EInternalReq (node == n);
         starA (anyA - EInternalReq true)),
        EExternalReq true,
        [| EExternalRsp (node == n && stat) |] ));
    (fun ?l:(n = (true : [%v: tNode])) ->
      ( (allA;
         EInternalReq (not (node == n));
         starA (anyA - EInternalReq true)),
        EExternalReq true,
        [| EExternalRsp (node == n && not stat) |] ));
  |]

let eExternalRsp ?l:(n = (true : [%v: tNode])) ?l:(st = (true : [%v: bool])) =
  (allA, EExternalRsp true, [||])

(* allow all session from internal node - UNCHANGED *)
let[@goal] p_firewall (n : tNode) =
  starA (anyA - EExternalReq (node == n) - EForwardReq (node == n));
  EInternalReq (node == n);
  allA;
  EExternalRsp (node == n && not stat);
  allA
