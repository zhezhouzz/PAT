event an_syn_writeRsp: tsyn_writeRsp;
event an_syn_readRsp: tsyn_readRsp;
event an_syn_putRsp: tsyn_putRsp;
event an_syn_putReq: tsyn_putReq;
event an_syn_getReq: tsyn_getReq;
event an_syn_commit: tsyn_commit;
event an_syn_abort: tsyn_abort;

spec strong_consistenty
observes an_syn_writeRsp, an_syn_readRsp
{
  var store: int;
  var is_init: bool;
  start state StartUp {
    entry {
      is_init = false;
    }

    on an_syn_writeRsp do (input: tsyn_writeRsp) {
      if (input.stat) {
        store = input.va;
        is_init = true;
      } 
    }

    on an_syn_readRsp do (input: tsyn_readRsp) {
      if (is_init) {
        assert (store == input.va), "spec violation";
      }
    }
  }
}