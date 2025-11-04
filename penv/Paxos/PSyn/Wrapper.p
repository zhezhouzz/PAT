fun acc1 (a: tAcceptorNode): bool {
  return a == Acceptor1;
}
fun acc2 (a: tAcceptorNode): bool {
  return a == Acceptor2;
}
fun pr1 (p: tProposerNode): bool {
  return p == Proposer1;
}
fun pr2 (p: tProposerNode): bool {
  return p == Proposer2;
}

fun send_eStart (src: machine, setting: setting, input: (proposer: tProposerNode, va: tVal)) {
  var dest: machine;
  dest = setting.proposerMap[input.proposer];
  send dest, syn_eStart, (controller = src, dst = dest, proposer = input.proposer, va = input.va);
}

fun send_eLostPrepareRsp (src: machine, setting: setting, input: (acceptor: tAcceptorNode, promised: tProposerNode)) {
  var dest: machine;
  dest = setting.acceptorMap[input.acceptor];
  send dest, syn_eLostPrepareRsp, (controller = src, dst = dest, acceptor = input.acceptor, promised = input.promised);
}

fun send_eLostPrepareReq (src: machine, setting: setting, input: (proposer: tProposerNode, acceptor: tAcceptorNode)) {
  var dest: machine;
  dest = setting.proposerMap[input.proposer];
  send dest, syn_eLostPrepareReq, (controller = src, dst = dest, proposer = input.proposer, acceptor = input.acceptor);
}

fun send_eLostAcceptRsp (src: machine, setting: setting, input: (proposer: tProposerNode, acceptor: tAcceptorNode)) {
  var dest: machine;
  dest = setting.acceptorMap[input.acceptor];
  send dest, syn_eLostAcceptRsp, (controller = src, dst = dest, proposer = input.proposer, acceptor = input.acceptor);
}

fun send_eLostAcceptReq (src: machine, setting: setting, input: (proposer: tProposerNode, acceptor: tAcceptorNode)) {
  var dest: machine;
  dest = setting.proposerMap[input.proposer];
  send dest, syn_eLostAcceptReq, (controller = src, dst = dest, proposer = input.proposer, acceptor = input.acceptor);
}



fun cast_syn_ePrepareRsp (input: tsyn_ePrepareRsp): (acceptor: tAcceptorNode, promised: tProposerNode, va: tVal, n_accepted: tProposerNode) {
  return (acceptor = input.acceptor, promised = input.promised, va = input.va, n_accepted = input.n_accepted);
}

fun cast_syn_ePrepareReq (input: tsyn_ePrepareReq): (proposer: tProposerNode, acceptor: tAcceptorNode, va: tVal) {
  return (proposer = input.proposer, acceptor = input.acceptor, va = input.va);
}

fun cast_syn_eLearn (input: tsyn_eLearn): (va: tVal) {
  return (va = input.va,);
}

fun cast_syn_eAcceptRsp (input: tsyn_eAcceptRsp): (proposer: tProposerNode, acceptor: tAcceptorNode, accepted: tProposerNode, va: tVal) {
  return (proposer = input.proposer, acceptor = input.acceptor, accepted = input.accepted, va = input.va);
}

fun cast_syn_eAcceptReq (input: tsyn_eAcceptReq): (proposer: tProposerNode, acceptor: tAcceptorNode, va: tVal) {
  return (proposer = input.proposer, acceptor = input.acceptor, va = input.va);
}



fun forward_syn_eStart (input: tsyn_eStart) {
  send input.dst, syn_eStart, input;
}

fun forward_syn_ePrepareRsp (input: tsyn_ePrepareRsp) {
  send input.dst, syn_ePrepareRsp, input;
}

fun forward_syn_ePrepareReq (input: tsyn_ePrepareReq) {
  send input.dst, syn_ePrepareReq, input;
}

fun forward_syn_eLostPrepareRsp (input: tsyn_eLostPrepareRsp) {
  send input.dst, syn_eLostPrepareRsp, input;
}

fun forward_syn_eLostPrepareReq (input: tsyn_eLostPrepareReq) {
  send input.dst, syn_eLostPrepareReq, input;
}

fun forward_syn_eLostAcceptRsp (input: tsyn_eLostAcceptRsp) {
  send input.dst, syn_eLostAcceptRsp, input;
}

fun forward_syn_eLostAcceptReq (input: tsyn_eLostAcceptReq) {
  send input.dst, syn_eLostAcceptReq, input;
}

fun forward_syn_eLearn (input: tsyn_eLearn) {
  send input.dst, syn_eLearn, input;
}

fun forward_syn_eAcceptRsp (input: tsyn_eAcceptRsp) {
  send input.dst, syn_eAcceptRsp, input;
}

fun forward_syn_eAcceptReq (input: tsyn_eAcceptReq) {
  send input.dst, syn_eAcceptReq, input;
}
