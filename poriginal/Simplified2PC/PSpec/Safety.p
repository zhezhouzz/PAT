spec strong_consistenty
observes syn_readRsp, syn_writeRsp
{
  var store: int;
  var is_init: bool;
  start state StartUp {
    entry {
      is_init = false;
    }

    on syn_writeRsp do (input: tsyn_writeRsp) {
      if (input.stat) {
        store = input.va;
        is_init = true;
      } 
    }

    on syn_readRsp do (input: tsyn_readRsp) {
      if (is_init) {
        assert (store == input.va), "spec violation";
      }
    }
  }
}