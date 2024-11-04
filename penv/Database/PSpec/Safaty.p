spec ryw observes syn_writeRsp, syn_readRsp {
  var store: int;
  start state Init {
    entry{
      store = -1;
    }
    on syn_writeRsp do (input: tsyn_writeRsp) {
      if (input.st) {
        store = input.va;
      }
    }
    on syn_readRsp do (input: tsyn_readRsp) {
      if (input.st) {
        assert (store == input.va), "property violation";
      }
    }
  }
}