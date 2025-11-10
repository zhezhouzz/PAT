fun send_eStart (src: machine, dest: machine, input: (node: tNode)) {
  RealSend(dest, syn_eStart, (controller = src, dst = dest, node = input.node));
}

fun cast_syn_eInternalReq (input: tsyn_eInternalReq): (node: tNode) {
  return (node = input.node,);
}

fun cast_syn_eForwardReq (input: tsyn_eForwardReq): (node: tNode) {
  return (node = input.node,);
}

fun cast_syn_eExternalRsp (input: tsyn_eExternalRsp): (node: tNode, stat: bool) {
  return (node = input.node, stat = input.stat);
}

fun cast_syn_eExternalReq (input: tsyn_eExternalReq): (node: tNode) {
  return (node = input.node,);
}



fun forward_syn_eStart (input: tsyn_eStart) {
  send input.dst, syn_eStart, input;
}

fun forward_syn_eInternalReq (input: tsyn_eInternalReq) {
  send input.dst, syn_eInternalReq, input;
}

fun forward_syn_eForwardReq (input: tsyn_eForwardReq) {
  send input.dst, syn_eForwardReq, input;
}

fun forward_syn_eExternalRsp (input: tsyn_eExternalRsp) {
  send input.dst, syn_eExternalRsp, input;
}

fun forward_syn_eExternalReq (input: tsyn_eExternalReq) {
  send input.dst, syn_eExternalReq, input;
}