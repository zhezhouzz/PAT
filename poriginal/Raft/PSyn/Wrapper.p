fun send_eStart (src: machine, setting: setting) {
  var dest: machine;
  dest = setting.initnode;
  send dest, syn_eStart, (controller = src, dst = dest);
}

fun send_eShutDown (src: machine, setting: setting) {
  var dest: machine;
  dest = setting.initnode;
  if($) {
    send dest, syn_eShutDown, (controller = src, dst = dest);
  } else {
    send setting.nodes, syn_eShutDown, (controller = src, dst = dest);
  }
}

fun send_eClientPutRsp (src: machine, setting: setting, input: (va: tVal, stat: bool)) {
  var dest: machine;
  dest = src;
  send dest, syn_eClientPutRsp, (controller = src, dst = dest, va = input.va, stat = input.stat);
}

fun send_eClientPut (src: machine, setting: setting, input: (va: tVal)) {
  var dest: machine;
  dest = setting.initnode;
  send dest, syn_eClientPut, (controller = src, dst = dest, va = input.va);
}

fun cast_syn_eVoteRsp (input: tsyn_eVoteRsp): (src: tNode, dest: tNode, stat: bool) {
  return (src = input.src, dest = input.dest, stat = input.stat);
}

fun cast_syn_eVoteReq (input: tsyn_eVoteReq): (src: tNode, dest: tNode, leader: tNode) {
  return (src = input.src, dest = input.dest, leader = input.leader);
}

fun cast_syn_eTimeout (input: tsyn_eTimeout): (dest: tNode) {
  return (dest = input.dest,);
}

fun cast_syn_eBecomeLeader (input: tsyn_eBecomeLeader): (leader: tNode) {
  return (leader = input.leader,);
}

fun cast_syn_eAppendEntry (input: tsyn_eAppendEntry): (node: tNode, va: tVal) {
  return (node = input.node, va = input.va);
}



fun forward_syn_eVoteRsp (input: tsyn_eVoteRsp) {
  send input.dst, syn_eVoteRsp, input;
}

fun forward_syn_eVoteReq (input: tsyn_eVoteReq) {
  send input.dst, syn_eVoteReq, input;
}

fun forward_syn_eTimeout (input: tsyn_eTimeout) {
  send input.dst, syn_eTimeout, input;
}

fun forward_syn_eStart (input: tsyn_eStart) {
  send input.dst, syn_eStart, input;
}

fun forward_syn_eShutDown (input: tsyn_eShutDown) {
  send input.dst, syn_eShutDown, input;
}

fun forward_syn_eClientPutRsp (input: tsyn_eClientPutRsp) {
  send input.dst, syn_eClientPutRsp, input;
}

fun forward_syn_eClientPut (input: tsyn_eClientPut) {
  send input.dst, syn_eClientPut, input;
}

fun forward_syn_eBecomeLeader (input: tsyn_eBecomeLeader) {
  send input.dst, syn_eBecomeLeader, input;
}

fun forward_syn_eAppendEntry (input: tsyn_eAppendEntry) {
  send input.dst, syn_eAppendEntry, input;
}
