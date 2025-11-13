event an_syn_writeRsp: tsyn_writeReq;
event an_syn_readRsp: tsyn_readRsp;

spec ryw observes an_syn_writeRsp, an_syn_readRsp {
  var store: int;
  start state Init {
    entry{
      store = -1;
    }
    on an_syn_writeRsp do (input: tsyn_writeRsp) {
        store = input.va;
    }
    on an_syn_readRsp do (input: tsyn_readRsp) {
      if (input.st) {
        assert (store == input.va), "property violation";
      }
    }
  }
}