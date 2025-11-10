fun send_eStart (src: machine, dest: machine) {
  send dest, syn_eStart, (controller = src, dst = dest);
}

fun send_eNetworkError (src: machine, dest: machine, input: (trial: int)) {
  send dest, syn_eNetworkError, (controller = src, dst = dest, trial = input.trial);
}

fun cast_syn_ePongLost (input: tsyn_ePongLost): (trial: int) {
  return (trial = input.trial,);
}

fun cast_syn_ePong (input: tsyn_ePong): (trial: int) {
  return (trial = input.trial,);
}

fun cast_syn_ePing (input: tsyn_ePing): (trial: int) {
  return (trial = input.trial,);
}


fun forward_syn_eStart (input: tsyn_eStart) {
  send input.dst, syn_eStart, input;
}

fun forward_syn_eShutDown (input: tsyn_eShutDown) {
  send input.dst, syn_eShutDown, input;
}

fun forward_syn_ePongLost (input: tsyn_ePongLost) {
  send input.dst, syn_ePongLost, input;
}

fun forward_syn_ePong (input: tsyn_ePong) {
  send input.dst, syn_ePong, input;
}

fun forward_syn_ePing (input: tsyn_ePing) {
  send input.dst, syn_ePing, input;
}

fun forward_syn_eNetworkError (input: tsyn_eNetworkError) {
  send input.dst, syn_eNetworkError, input;
}