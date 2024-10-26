(* type tNode = (node1 * node2[@tNode]) *)

val ( == ) : 'a -> 'a -> bool
val eStart : < node : (node1 * node2[@tNode]) > [@@gen]
val eInternalReq : < node : (node1 * node2[@tNode]) > [@@obs]
val eForwardReq : < node : (node1 * node2[@tNode]) > [@@obs]
val eExternalReq : < node : (node1 * node2[@tNode]) > [@@obs]
val eExternalRsp : < node : (node1 * node2[@tNode]) ; stat : bool > [@@obs]

let eStart =
  [|
    (fun ?l:(n = (true : [%v: (node1 * node2[@tNode])])) ->
      (allA, EStart (node == n), [| EInternalReq (node == n) |]));
    (* (fun ?l:(n = (true : [%v: (node1 * node2[@tNode])])) -> *)
    (*   (allA, EStart (node == n), [||])); *)
  |]

let eInternalReq ?l:(n = (true : [%v: (node1 * node2[@tNode])])) =
  ( starA
      (anyA
      - EExternalReq (node == n)
      - EInternalReq (node == n)
      - EForwardReq (node == n)),
    EInternalReq (node == n),
    [| EForwardReq (node == n) |] )

let eForwardReq =
  [|
    (fun ?l:(n = (true : [%v: (node1 * node2[@tNode])])) ->
      ( starA (anyA - EForwardReq (node == n)),
        EForwardReq (node == n),
        [| EExternalReq (node == n) |] ));
    (* (fun ?l:(n = (true : [%v: (node1 * node2[@tNode])])) -> *)
    (*   (starA (anyA - EForwardReq (node == n)), EForwardReq (node == n), [||])); *)
  |]

let eExternalReq =
  [|
    (fun ?l:(n = (true : [%v: (node1 * node2[@tNode])])) ->
      ( (allA;
         EInternalReq (node == n);
         starA (anyA - EInternalReq true)),
        EExternalReq (node == n),
        [| EExternalRsp (node == n && stat) |] ));
    (* (fun ?l:(n = (true : [%v: (node1 * node2[@tNode])])) -> *)
    (*   ( starA (anyA - EInternalReq (node == n)), *)
    (*     EExternalReq (node == n), *)
    (*     [| EExternalRsp (node == n && not stat) |] )); *)
    (fun ?l:(n = (true : [%v: (node1 * node2[@tNode])])) ->
      ( (allA;
         EInternalReq (not (node == n));
         starA (anyA - EInternalReq true)),
        EExternalReq (node == n),
        [| EExternalRsp (node == n && not stat) |] ));
  |]

let eExternalRsp ?l:(n = (true : [%v: (node1 * node2[@tNode])]))
    ?l:(st = (true : [%v: bool])) =
  (allA, EExternalRsp (node == n && stat == st), [||])

let[@goal] allow_all_session_from_internal_node (n : (node1 * node2[@tNode])) =
  not
    (starA (anyA - EExternalReq (node == n) - EForwardReq (node == n));
     EInternalReq (node == n);
     (* allA; *)
     (* EExternalReq (node == n); *)
     allA;
     EExternalRsp (node == n && not stat);
     allA)
