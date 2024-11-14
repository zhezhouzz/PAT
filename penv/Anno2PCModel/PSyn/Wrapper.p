type setting = machine;
fun send_eUpdateReq (src: machine, dest: machine, input: (gid: tGid, key: tKey, value: tVal)) {
    send dest, syn_eUpdateReq, (controller = src, dst = dest, gid = input.gid, key = input.key, val = input.value);
  }

fun send_eStartTxnReq (src: machine, dest: machine) {
    send dest, syn_eStartTxnReq, (controller = src, dst = dest, client = src);
  }

fun send_eRollbackTxnReq (src: machine, dest: machine, input: (gid: tGid)) {
    send dest, syn_eRollbackTxnReq, (controller = src, dst = dest, gid = input.gid);
  }

fun send_eReadReq (src: machine, dest: machine, input: (gid: tGid, key: tKey)) {
    send dest, syn_eReadReq, (controller = src, dst = dest, gid = input.gid, key = input.key);
  }

fun send_eCommitTxnReq (src: machine, dest: machine, input: (gid: tGid)) {
    send dest, syn_eCommitTxnReq, (controller = src, dst = dest, gid = input.gid);
  }


fun cast_syn_eUpdateRsp (input: tsyn_eUpdateRsp): (gid: tGid, key: tKey, value: tVal, status: tCmdStatus) {
    return (gid = input.gid, key = input.key, value = input.val, status = input.status);
  }

fun cast_syn_eStartTxnRsp (input: tsyn_eStartTxnRsp): (gid: tGid) {
    return (gid = input.gid,);
  }

fun cast_syn_eShardUpdateKeyRsp (input: tsyn_eShardUpdateKeyRsp): (gid: tGid, key: tKey, value: tVal, status: tCmdStatus) {
    return (gid = input.gid, key = input.key, value = input.val, status = input.status);
  }

fun cast_syn_eShardUpdateKeyReq (input: tsyn_eShardUpdateKeyReq): (gid: tGid, key: tKey, value: tVal) {
    return (gid = input.gid, key = input.key, value = input.val);
  }

fun cast_syn_eShardReadKeyRsp (input: tsyn_eShardReadKeyRsp): (gid: tGid, key: tKey, value: tVal, status: tCmdStatus) {
    return (gid = input.gid, key = input.key, value = input.val, status = input.status);
  }

fun cast_syn_eShardReadKeyReq (input: tsyn_eShardReadKeyReq): (gid: tGid, key: tKey) {
    return (gid = input.gid, key = input.key);
  }

fun cast_syn_eShardPrepareRsp (input: tsyn_eShardPrepareRsp): (gid: tGid, bstatus: bool) {
    var bstatus: bool;
    if (input.status == SHARD_OK) {
        bstatus = true;
    } else {
        bstatus = false;
    }
    return (gid = input.gid, bstatus = bstatus);
  }

fun cast_syn_eShardPrepareReq (input: tsyn_eShardPrepareReq): (gid: tGid) {
    return (gid = input.gid,);
  }

fun cast_syn_eShardCommitTxn (input: tsyn_eShardCommitTxn): (gid: tGid) {
    return (gid = input.gid,);
  }

fun cast_syn_eShardAbortTxn (input: tsyn_eShardAbortTxn): (gid: tGid) {
    return (gid = input.gid,);
  }

fun cast_syn_eReadRsp (input: tsyn_eReadRsp): (gid: tGid, key: tKey, value: tVal, status: tCmdStatus) {
    return (gid = input.gid, key = input.key, value = input.val, status = input.status);
  }

fun cast_syn_eCommitTxnRsp (input: tsyn_eCommitTxnRsp): (gid: tGid, txnstatus: tTxnStatus) {
    return (gid = input.gid, txnstatus = input.status);
  }

fun forward_syn_eUpdateRsp (input: tsyn_eUpdateRsp) {
    send input.dst, syn_eUpdateRsp, input;
}

fun forward_syn_eUpdateReq (input: tsyn_eUpdateReq) {
    send input.dst, syn_eUpdateReq, input;
  }

fun forward_syn_eStartTxnRsp (input: tsyn_eStartTxnRsp) {
    send input.dst, syn_eStartTxnRsp, input;
  }

fun forward_syn_eStartTxnReq (input: tsyn_eStartTxnReq) {
    send input.dst, syn_eStartTxnReq, input;
  }

fun forward_syn_eShardUpdateKeyRsp (input: tsyn_eShardUpdateKeyRsp) {
    send input.dst, syn_eShardUpdateKeyRsp, input;
  }

fun forward_syn_eShardUpdateKeyReq (input: tsyn_eShardUpdateKeyReq) {
    send input.dst, syn_eShardUpdateKeyReq, input;
  }

fun forward_syn_eShardReadKeyRsp (input: tsyn_eShardReadKeyRsp) {
    send input.dst, syn_eShardReadKeyRsp, input;
  }

fun forward_syn_eShardReadKeyReq (input: tsyn_eShardReadKeyReq) {
    send input.dst, syn_eShardReadKeyReq, input;
  }

fun forward_syn_eShardPrepareRsp (input: tsyn_eShardPrepareRsp) {
    send input.dst, syn_eShardPrepareRsp, input;
  }

fun forward_syn_eShardPrepareReq (input: tsyn_eShardPrepareReq) {
    send input.dst, syn_eShardPrepareReq, input;
  }

fun forward_syn_eShardCommitTxn (input: tsyn_eShardCommitTxn) {
    send input.dst, syn_eShardCommitTxn, input;
  }

fun forward_syn_eShardAbortTxn (input: tsyn_eShardAbortTxn) {
    send input.dst, syn_eShardAbortTxn, input;
  }

fun forward_syn_eRollbackTxnReq (input: tsyn_eRollbackTxnReq) {
    send input.dst, syn_eRollbackTxnReq, input;
  }

fun forward_syn_eReadRsp (input: tsyn_eReadRsp) {
    send input.dst, syn_eReadRsp, input;
  }

fun forward_syn_eReadReq (input: tsyn_eReadReq) {
    send input.dst, syn_eReadReq, input;
  }

fun forward_syn_eCommitTxnRsp (input: tsyn_eCommitTxnRsp) {
    send input.dst, syn_eCommitTxnRsp, input;
  }

fun forward_syn_eCommitTxnReq (input: tsyn_eCommitTxnReq) {
    send input.dst, syn_eCommitTxnReq, input;
  }