event an_syn_writeRsp: tsyn_writeRsp;
event an_syn_readRsp: tsyn_readRsp;
event an_syn_writeToMid: tsyn_writeToMid;
event an_syn_writeToTail: tsyn_writeToTail;
event an_syn_writeReq: tsyn_writeReq;
event an_syn_readReq: tsyn_readReq;
event an_syn_crashTail: tsyn_crashTail;

spec ryw observes an_syn_writeRsp, an_syn_readRsp {
  var store: map[tKey, int];
  start state Init {
    entry{}
    on an_syn_writeRsp do (input: tsyn_writeRsp) {
      store[input.key] = input.va;
    }
    on an_syn_readRsp do (input: tsyn_readRsp) {
      if (input.st) {
          assert (input.va == store[input.key]), "property violation"; 
      }
    }
  }
}