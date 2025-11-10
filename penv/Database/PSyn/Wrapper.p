fun send_writeReq (src: machine, dest: machine, input: (va: int)) {
    RealSend(dest, syn_writeReq, (controller = src, dst = dest, va = input.va));
  }

fun send_readReq (src: machine, dest: machine) {
    RealSend(dest, syn_readReq, (controller = src, dst = dest));
  }

fun cast_syn_writeRsp (input: tsyn_writeRsp): (va: int) {
    return (va = input.va,);
  }

fun cast_syn_readRsp (input: tsyn_readRsp): (va: int, st: bool) {
    return (va = input.va, st = input.st);
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
