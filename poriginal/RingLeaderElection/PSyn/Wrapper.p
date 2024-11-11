fun send_eWakeup (src: machine, ring: setting, input: (node: tNode)) {
  RealSend(ring[input.node], syn_eWakeup, (controller = src, dst = ring[input.node], node = input.node));
}

fun cast_syn_eWon (input: tsyn_eWon): (leader: tNode) {
  return (leader = input.leader,);
}

fun cast_syn_eNominate (input: tsyn_eNominate): (node: tNode, leader: tNode) {
  return (node = input.node, leader = input.leader);
}



fun forward_syn_eWon (input: tsyn_eWon) {
  send input.dst, syn_eWon, input;
}

fun forward_syn_eWakeup (input: tsyn_eWakeup) {
  send input.dst, syn_eWakeup, input;
}

fun forward_syn_eNominate (input: tsyn_eNominate) {
  send input.dst, syn_eNominate, input;
}
