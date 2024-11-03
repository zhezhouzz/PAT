fun send_eStart (src: machine, dest: machine) {
    send dest, eStart, (controller = src,);
  }

fun send_eNetworkError (src: machine, dest: machine, input: (trial: int)) {
    send dest, eNetworkError, (controller = src, trial = input.trial);
  }

fun cast_inePongLost (input: tinePongLost): (trial: int) {
    return (trial = input.trial,);
  }

fun cast_inePong (input: tinePong): (trial: int) {
    return (trial = input.trial,);
  }

fun cast_inePing (input: tinePing): (trial: int) {
    return (trial = input.trial,);
  }

fun forward_inePongLost (input: tinePongLost) {
    send input.dst, inePongLost, input;
  }

fun forward_inePong (input: tinePong) {
    send input.dst, inePong, input;
  }

fun forward_inePing (input: tinePing) {
    send input.dst, inePing, input;
  }