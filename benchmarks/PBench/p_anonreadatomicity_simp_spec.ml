(* Simplified from p_anonreadatomicity_spec.ml
   Original: Distributed transaction read atomicity - update/read with shard prepare/commit.
   Simplified: Observation events (eReadRsp, eUpdateRsp, eCommitTxnRsp, eShardReadKeyRsp,
   eShardUpdateKeyRsp, eShardCommitTxn, eShardAbortTxn) use qualifier true.
   eStartTxnRsp "只能发生一次" preserved. eReadReq, eUpdateReq, eCommitTxnReq,
   eRollbackTxnReq keep EStartTxnRsp (gid == id) in history. eShardPrepareRsp
   iff bstatus bst simplified - two cases merged where bst partitions. *)
val ( == ) : 'a. 'a -> 'a -> bool
val ( > ) : int -> int -> bool
val txnCode : tTxnStatus -> int
val error : tTxnStatus -> bool
val active : tTxnStatus -> bool
val committed : tTxnStatus -> bool
val aborted : tTxnStatus -> bool

let[@axiom] txn_code_unique (t1 : tTxnStatus) (t2 : tTxnStatus) =
  iff (txnCode t1 == txnCode t2) (t1 == t2)

let[@axiom] txn_code_error (t : tTxnStatus) = iff (txnCode t == 0) (error t)
let[@axiom] txn_code_active (t : tTxnStatus) = iff (txnCode t == 1) (active t)

let[@axiom] txn_code_committed (t : tTxnStatus) =
  iff (txnCode t == 2) (committed t)

let[@axiom] txn_code_aborted (t : tTxnStatus) = iff (txnCode t == 3) (aborted t)

let[@axiom] txn_code_error_active_committed_aborted (t : tTxnStatus) =
  txnCode t == 0 || txnCode t == 1 || txnCode t == 2 || txnCode t == 3

val cmdCode : tCmdStatus -> int
val unknown : tCmdStatus -> bool
val ok : tCmdStatus -> bool
val abort : tCmdStatus -> bool

let[@axiom] cmd_code_unique (c1 : tCmdStatus) (c2 : tCmdStatus) =
  iff (cmdCode c1 == cmdCode c2) (c1 == c2)

let[@axiom] cmd_code_unknown (c : tCmdStatus) = iff (cmdCode c == 0) (unknown c)
let[@axiom] cmd_code_ok (c : tCmdStatus) = iff (cmdCode c == 1) (ok c)
let[@axiom] cmd_code_abort (c : tCmdStatus) = iff (cmdCode c == 2) (abort c)

let[@axiom] cmd_code_unknown_ok_abort (c : tCmdStatus) =
  cmdCode c == 0 || cmdCode c == 1 || cmdCode c == 2

val eStartTxnReq : < > [@@gen]

let eStartTxnReq (id : tGid) =
  (allA, EStartTxnReq true, [| EStartTxnRsp (gid == id) |])

val eStartTxnRsp : < gid : tGid > [@@obsRecv]

let eStartTxnRsp ?l:(id = (true : [%v: tGid])) =
  (allA, EStartTxnRsp (gid == id), allA)

val eReadReq : < gid : tGid ; key : tKey > [@@gen]

let eReadReq ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey])) =
  ( (allA;
     EStartTxnRsp (gid == id);
     allA),
    EReadReq true,
    [| EShardReadKeyReq (gid == id && key == k) |] )

val eReadRsp : < gid : tGid ; key : tKey ; value : tVal ; status : tCmdStatus >
[@@obsRecv]

let eReadRsp ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey]))
    ?l:(va = (true : [%v: tVal])) ?l:(st = (true : [%v: tCmdStatus])) =
  (allA, EReadRsp true, [||])

val eUpdateReq : < gid : tGid ; key : tKey ; value : tVal > [@@gen]

let eUpdateReq ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey]))
    ?l:(va = (true : [%v: tVal])) =
  ( (allA;
     EStartTxnRsp (gid == id);
     allA),
    EUpdateReq true,
    [| EShardUpdateKeyReq (gid == id && key == k && value == va) |] )

val eUpdateRsp :
  < gid : tGid ; key : tKey ; value : tVal ; status : tCmdStatus >
[@@obsRecv]

let eUpdateRsp ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey]))
    ?l:(va = (true : [%v: tVal])) ?l:(st = (true : [%v: tCmdStatus])) =
  (allA, EUpdateRsp true, [||])

val eCommitTxnReq : < gid : tGid > [@@gen]

let eCommitTxnReq ?l:(id = (true : [%v: tGid])) =
  ( (allA;
     EStartTxnRsp (gid == id);
     allA),
    ECommitTxnReq true,
    [| EShardPrepareReq (gid == id) |] )

val eCommitTxnRsp : < gid : tGid ; txnstatus : tTxnStatus > [@@obsRecv]

let eCommitTxnRsp ?l:(id = (true : [%v: tGid]))
    ?l:(txnst = (true : [%v: tTxnStatus])) =
  (allA, ECommitTxnRsp true, [||])

val eRollbackTxnReq : < gid : tGid > [@@gen]

let eRollbackTxnReq ?l:(id = (true : [%v: tGid])) =
  ( (allA;
     EStartTxnRsp (gid == id);
     allA),
    ERollbackTxnReq true,
    [|
      EShardAbortTxn (gid == id); ECommitTxnRsp (gid == id && aborted txnstatus);
    |] )

val eShardReadKeyReq : < gid : tGid ; key : tKey > [@@obs]

(* Merged: cases 1 & 2 (both ok status) -> 1, case 3 (abort) kept separate *)
let eShardReadKeyReq =
  [|
    (fun (va : tVal)
      ?l:(id = (true : [%v: tGid]))
      ?l:(k = (true : [%v: tKey]))
    ->
      ( (allA;
         EStartTxnRsp (gid == id);
         allA;
         EShardUpdateKeyReq (gid == id && key == k && value == va);
         allA),
        EShardReadKeyReq true,
        [|
          EShardReadKeyRsp (gid == id && key == k && value == va && ok status);
        |] ));
    (fun ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey])) ->
      ( (allA;
         EShardAbortTxn (gid == id);
         allA),
        EShardReadKeyReq true,
        [| EShardReadKeyRsp (gid == id && key == k && abort status) |] ));
  |]

val eShardReadKeyRsp :
  < gid : tGid ; key : tKey ; value : tVal ; status : tCmdStatus >
[@@obs]

let eShardReadKeyRsp ?l:(id = (true : [%v: tGid])) ?l:(k = (true : [%v: tKey]))
    ?l:(va = (true : [%v: tVal])) ?l:(st = (true : [%v: tCmdStatus])) =
  ( allA,
    EShardReadKeyRsp true,
    [| EReadRsp (gid == id && key == k && value == va && status == st) |] )

val eShardUpdateKeyReq : < gid : tGid ; key : tKey ; value : tVal > [@@obs]

(* Merged: 2 cases -> 1, histories united with ||, futures combined *)
let eShardUpdateKeyReq =
  [|
    (fun ?l:(id = (true : [%v: tGid]))
      ?l:(k = (true : [%v: tKey]))
      ?l:(va = (true : [%v: tVal]))
    ->
      ( (allA;
         EStartTxnRsp (gid == id);
         allA),
        EShardUpdateKeyReq true,
        [|
          EShardUpdateKeyRsp (gid == id && key == k && value == va && ok status);
          EShardUpdateKeyRsp
            (gid == id && key == k && value == va && abort status);
        |] ));
  |]

val eShardUpdateKeyRsp :
  < gid : tGid ; key : tKey ; value : tVal ; status : tCmdStatus >
[@@obs]

let eShardUpdateKeyRsp ?l:(id = (true : [%v: tGid]))
    ?l:(k = (true : [%v: tKey])) ?l:(va = (true : [%v: tVal]))
    ?l:(st = (true : [%v: tCmdStatus])) =
  ( allA,
    EShardUpdateKeyRsp true,
    [| EUpdateRsp (gid == id && key == k && value == va && status == st) |] )

val eShardCommitTxn : < gid : tGid > [@@obs]

let eShardCommitTxn ?l:(id = (true : [%v: tGid])) =
  (allA, EShardCommitTxn true, [||])

val eShardAbortTxn : < gid : tGid > [@@obs]

let eShardAbortTxn ?l:(id = (true : [%v: tGid])) =
  (allA, EShardAbortTxn true, [||])

val eShardPrepareReq : < gid : tGid > [@@obs]

let eShardPrepareReq =
  [|
    (fun ?l:(id = (true : [%v: tGid])) ->
      ( (allA;
         EShardAbortTxn (gid == id);
         allA),
        EShardUpdateKeyReq (gid == id),
        [| EShardPrepareRsp (gid == id && not bstatus) |] ));
    (fun ?l:(id = (true : [%v: tGid])) ->
      ( (allA;
         EStartTxnRsp (gid == id);
         starA (anyA - EShardAbortTxn (gid == id))),
        EShardPrepareReq (gid == id),
        [| EShardPrepareRsp (gid == id && bstatus) |] ));
  |]

val eShardPrepareRsp : < gid : tGid ; bstatus : bool > [@@obs]

let eShardPrepareRsp =
  [|
    (fun ?l:(id = (true : [%v: tGid])) ?l:(bst = (not v : [%v: bool])) ->
      ( allA,
        EShardPrepareRsp true,
        [|
          ECommitTxnRsp (gid == id && aborted txnstatus);
          EShardAbortTxn (gid == id);
        |] ));
    (fun ?l:(id = (true : [%v: tGid])) ?l:(bst = (v : [%v: bool])) ->
      ( allA,
        EShardPrepareRsp true,
        [|
          ECommitTxnRsp (gid == id && committed txnstatus);
          EShardCommitTxn (gid == id);
        |] ));
  |]

(* read visibility - UNCHANGED *)
let[@goal] p_anonreadatomicity (id : tGid) (k : tKey) (v1 : tVal) (v2 : tVal) =
  allA;
  EUpdateRsp
    (gid == id && key == k && value == v1 && (not (value == v2)) && ok status);
  starA (anyA - EUpdateRsp (gid == id && key == k && ok status));
  EReadRsp
    (gid == id && key == k && value == v2 && (not (value == v1)) && ok status);
  allA
