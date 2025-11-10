fun send_writeReq (src: machine, dest: machine, input: (va: int)) {
    RealSend(dest, syn_writeReq, (controller = src, dst = dest, va = input.va));
  }

fun send_readReq (src: machine, dest: machine) {
    RealSend(dest, syn_readReq, (controller = src, dst = dest));
  }



fun cast_syn_writeRsp (input: tsyn_writeRsp): (va: int, stat: bool) {
    return (va = input.va, stat = input.stat);
  }

fun cast_syn_readRsp (input: tsyn_readRsp): (va: int) {
    return (va = input.va,);
  }

fun cast_syn_putRsp (input: tsyn_putRsp): (va: int, stat: bool) {
    return (va = input.va, stat = input.stat);
  }

fun cast_syn_putReq (input: tsyn_putReq): (va: int) {
    return (va = input.va,);
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

fun forward_syn_putRsp (input: tsyn_putRsp) {
    send input.dst, syn_putRsp, input;
  }

fun forward_syn_putReq (input: tsyn_putReq) {
    send input.dst, syn_putReq, input;
  }

fun forward_syn_getReq (input: tsyn_getReq) {
    send input.dst, syn_getReq, input;
  }

fun forward_syn_commit (input: tsyn_commit) {
    send input.dst, syn_commit, input;
  }

fun forward_syn_abort (input: tsyn_abort) {
    send input.dst, syn_abort, input;
  }