spec ryw observes syn_writeRsp, syn_readRsp {
  var store: map[tKey, int];
  start state Init {
    entry{}
    on syn_writeRsp do (input: tsyn_writeRsp) {
      store[input.key] = input.va;
    }
    on syn_readRsp do (input: tsyn_readRsp) {
      if (input.st) {
          assert (input.va != store[input.key]), "property violation"; 
      }
    }
  }
}