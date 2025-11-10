fun send_writeReq (src: machine, dest: (head: machine, mid: machine, tail: machine), input: (key: tKey, va: int)) {
  send dest.head, syn_writeReq, (controller = src, dst = dest.head, key = input.key, va = input.va);
}

fun send_readReq (src: machine, dest: (head: machine, mid: machine, tail: machine), input: (key: tKey)) {
  send dest.tail, syn_readReq, (controller = src, dst = dest.tail, key = input.key);
}

fun send_crashTail (src: machine, dest: (head: machine, mid: machine, tail: machine)) {
  send dest.tail, syn_crashTail, (controller = src, dst = dest.tail);
}



fun cast_syn_writeToTail (input: tsyn_writeToTail): (key: tKey, va: int) {
  return (key = input.key, va = input.va);
}

fun cast_syn_writeToMid (input: tsyn_writeToMid): (key: tKey, va: int, node: tNode) {
  return (key = input.key, va = input.va, node = input.node);
}

fun cast_syn_writeRsp (input: tsyn_writeRsp): (key: tKey, va: int) {
  return (key = input.key, va = input.va);
}

fun cast_syn_readRsp (input: tsyn_readRsp): (key: tKey, va: int, st: bool) {
  return (key = input.key, va = input.va, st = input.st);
}



fun forward_syn_writeToTail (input: tsyn_writeToTail) {
  send input.dst, syn_writeToTail, input;
}

fun forward_syn_writeToMid (input: tsyn_writeToMid) {
  send input.dst, syn_writeToMid, input;
}

fun forward_syn_writeRsp (input: tsyn_writeRsp) {
  send input.dst, syn_writeRsp, input;
}

fun forward_syn_writeReq (input: tsyn_writeReq) {
  send input.dst, syn_writeReq, input;
}

fun forward_syn_readRsp (input: tsyn_readRsp) {
  send input.dst, syn_readRsp, input;
}

fun forward_syn_readReq (input: tsyn_readReq) {
  send input.dst, syn_readReq, input;
}

fun forward_syn_crashTail (input: tsyn_crashTail) {
  send input.dst, syn_crashTail, input;
}