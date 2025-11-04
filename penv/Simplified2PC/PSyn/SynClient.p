machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_bool: set[bool], domain_int: set[int])) {
      var setting: setting;
      var domain_bool: set[bool];
      var domain_int: set[int];
      var __x0: int;
      var input_putReq: (va: int);
      var __y1: int;
      var input_putRsp: (va: int, stat: bool);
      var __y2: int;
      var __y3: bool;
      var input_writeRsp: (va: int, stat: bool);
      var __y4: int;
      var __y5: bool;
      var input_readRsp: (va: int);
      var __y6: int;
      setting = input.setting;
      domain_bool = input.domain_bool;
      domain_int = input.domain_int;
      while(true){
        __x0 = choose(domain_int);
        if ((__x0 >= 0)) {
          break;
        };
      };
      send_writeReq(this, setting, (va = __x0,));
      receive { case syn_putReq: (input: tsyn_putReq) {
        announce an_syn_putReq, input;
        forward_syn_putReq(input);
        input_putReq = cast_syn_putReq(input);
        __y1 = input_putReq.va;
      }};
      assert true;
      receive { case syn_putRsp: (input: tsyn_putRsp) {
        announce an_syn_putRsp, input;
        forward_syn_putRsp(input);
        input_putRsp = cast_syn_putRsp(input);
        __y2 = input_putRsp.va;
        __y3 = input_putRsp.stat;
      }};
      assert true;
      receive { case syn_writeRsp: (input: tsyn_writeRsp) {
        announce an_syn_writeRsp, input;
        input_writeRsp = cast_syn_writeRsp(input);
        __y4 = input_writeRsp.va;
        __y5 = input_writeRsp.stat;
      }};
      assert true;
      send_readReq(this, setting);
      receive { case syn_getReq: (input: tsyn_getReq) {
        announce an_syn_getReq, input;
        forward_syn_getReq(input);
        break;
      }};
      assert true;
      receive { case syn_readRsp: (input: tsyn_readRsp) {
        announce an_syn_readRsp, input;
        input_readRsp = cast_syn_readRsp(input);
        __y6 = input_readRsp.va;
      }};
      assert true;
      receive { case syn_commit: (input: tsyn_commit) {
        announce an_syn_commit, input;
        forward_syn_commit(input);
        break;
      }};
      assert true;
    }

  }
}

