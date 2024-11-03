type tsyn_eUpdateRsp = (controller:machine, dst:machine, router:machine, gid:tGid, key:tKey, val:tVal, status:tCmdStatus);
type tsyn_eUpdateReq = (controller:machine, dst:machine, gid:tGid, key:tKey, val:tVal);
type tsyn_eStartTxnRsp = (controller:machine, dst:machine, router: machine, gid:tGid, start_time: int);
type tsyn_eStartTxnReq = (controller:machine, dst:machine, client: machine);
type tsyn_eShardUpdateKeyRsp = (controller:machine, dst:machine, shard: machine, gid:tGid, key:tKey, val:tVal, status:tCmdStatus);
type tsyn_eShardUpdateKeyReq = (controller:machine, dst:machine, router:machine, gid:tGid, key:tKey, val:tVal, snapshot_time: tTime);
type tsyn_eShardReadKeyRsp = (controller:machine, dst:machine, shard: machine, gid:tGid, key:tKey, val:tVal, status:tCmdStatus);
type tsyn_eShardReadKeyReq = (controller:machine, dst:machine, router:machine, gid:tGid, key:tKey, snapshot_time: tTime);
type tsyn_eShardPrepareRsp = (controller:machine, dst:machine, shard: machine, gid:tGid, status: tShardPrepareStatus, prepare_time: tTime);
type tsyn_eShardPrepareReq = (controller:machine, dst:machine, router:machine, gid:tGid, lead_participant: machine);
type tsyn_eShardCommitTxn = (controller:machine, dst:machine, gid:tGid, commit_time: tTime);
type tsyn_eShardAbortTxn = (controller:machine, dst:machine, gid:tGid);
type tsyn_eRollbackTxnReq = (controller:machine, dst:machine, gid:tGid);
type tsyn_eReadRsp = (controller:machine, dst:machine, router:machine, gid:tGid, key:tKey, val:tVal, status:tCmdStatus);
type tsyn_eReadReq = (controller:machine, dst:machine, gid:tGid, key:tKey);
type tsyn_eCommitTxnRsp = (controller:machine, dst:machine, gid:tGid, status:tTxnStatus);
type tsyn_eCommitTxnReq = (controller:machine, dst:machine, gid:tGid);
event syn_eUpdateRsp: tsyn_eUpdateRsp;
event syn_eUpdateReq: tsyn_eUpdateReq;
event syn_eStartTxnRsp: tsyn_eStartTxnRsp;
event syn_eStartTxnReq: tsyn_eStartTxnReq;
event syn_eShardUpdateKeyRsp: tsyn_eShardUpdateKeyRsp;
event syn_eShardUpdateKeyReq: tsyn_eShardUpdateKeyReq;
event syn_eShardReadKeyRsp: tsyn_eShardReadKeyRsp;
event syn_eShardReadKeyReq: tsyn_eShardReadKeyReq;
event syn_eShardPrepareRsp: tsyn_eShardPrepareRsp;
event syn_eShardPrepareReq: tsyn_eShardPrepareReq;
event syn_eShardCommitTxn: tsyn_eShardCommitTxn;
event syn_eShardAbortTxn: tsyn_eShardAbortTxn;
event syn_eRollbackTxnReq: tsyn_eRollbackTxnReq;
event syn_eReadRsp: tsyn_eReadRsp;
event syn_eReadReq: tsyn_eReadReq;
event syn_eCommitTxnRsp: tsyn_eCommitTxnRsp;
event syn_eCommitTxnReq: tsyn_eCommitTxnReq;

fun do_send_eShardReadKeyRsp(controller: machine, dst: machine, old: tShardReadKeyRsp){
    send controller, syn_eShardReadKeyRsp, (controller = controller, dst = dst, shard=old.shard, gid=old.gid, key = old.key, val = old.val, status = old.status);    
}

fun do_send_eShardUpdateKeyRsp(controller: machine, dst: machine, old: tShardUpdateKeyRsp){
    send controller, syn_eShardUpdateKeyRsp, (controller = controller, dst = dst, shard=old.shard, gid=old.gid, key = old.key, val = old.val, status = old.status);    
}

fun do_send_eShardPrepareRsp(controller: machine, dst: machine, old: tShardPrepareRsp){
    send controller, syn_eShardPrepareRsp, (controller = controller, dst = dst, shard=old.shard, gid=old.gid, status = old.status, prepare_time = old.prepare_time);    
}

fun do_send_eStartTxnRsp(controller: machine, dst: machine, old: tStartTxnRsp){
    send controller, syn_eStartTxnRsp, (controller = controller, dst = dst, router=old.router, gid=old.gid, start_time=old.start_time);    
}

fun do_send_eUpdateRsp (controller: machine, dst: machine, old_payload: tUpdateRsp) {
    send controller, syn_eUpdateRsp, (controller = controller, dst = dst, router=old_payload.router, gid=old_payload.gid, key=old_payload.key, val=old_payload.val, status=old_payload.status); 
}

fun do_send_eShardReadKeyReq(controller: machine, dst: machine, old: tShardReadKeyReq){
    send controller, syn_eShardReadKeyReq, (controller = controller, dst = dst, router=old.router, gid=old.gid, key=old.key, snapshot_time=old.snapshot_time);    
}

fun do_send_eReadRsp(controller: machine, dst: machine, old: tReadRsp){
    send controller, syn_eReadRsp, (controller = controller, dst = dst, router=old.router, gid=old.gid, key=old.key, val = old.val, status = old.status);    
}

fun do_send_eShardUpdateKeyReq(controller: machine, dst: machine, old: tShardUpdateKeyReq){
    send controller, syn_eShardUpdateKeyReq, (controller = controller, dst = dst, router=old.router, gid=old.gid, key=old.key, val = old.val, snapshot_time=old.snapshot_time);    
}

fun do_send_eShardPrepareReq(controller: machine, dst: machine, old: tShardPrepareReq){
    send controller, syn_eShardPrepareReq, (controller = controller, dst = dst, router=old.router, gid=old.gid, lead_participant = old.lead_participant);    
}

fun do_send_eCommitTxnRsp(dst: machine, old: tCommitTxnRsp){
    send dst, syn_eCommitTxnRsp, (controller = dst, dst = dst, gid=old.gid, status = old.status);    
}

fun do_send_eShardCommitTxn(dst: machine, old: tShardCommitTxn){
    send dst, syn_eShardCommitTxn, (controller = dst, dst = dst, gid=old.gid, commit_time = old.commit_time);    
}

fun do_send_eShardAbortTxn(dst: machine, old: tShardAbortTxn){
    send dst, syn_eShardAbortTxn, (controller = dst, dst = dst, gid=old.gid);    
}