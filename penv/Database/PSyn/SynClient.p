machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_bool: set[bool], domain_int: set[int])) {
      var setting: setting;
      var domain_bool: set[bool];
      var domain_int: set[int];
      var __x0: int;
      var input_writeRsp: (va: int);
      var __y1: int;
      var __x2: int;
      var input_readRsp: (va: int, st: bool);
      var __y3: int;
      var __y4: bool;
      var __y5: int;
      setting = input.setting;
      domain_bool = input.domain_bool;
      domain_int = input.domain_int;
      while(true){
        __x0 = choose(domain_int);
        if (true) {
          break;
        };
      };
      send_writeReq(this, setting, (va = __x0,));
      receive { case syn_writeRsp: (input: tsyn_writeRsp) {
        announce an_syn_writeRsp, input;
        input_writeRsp = cast_syn_writeRsp(input);
        __y1 = input_writeRsp.va;
      }};
      assert true;
      while(true){
        __x2 = choose(domain_int);
        if (!((__x0 == __x2))) {
          break;
        };
      };
      send_writeReq(this, setting, (va = __x2,));
      send_readReq(this, setting);
      receive { case syn_readRsp: (input: tsyn_readRsp) {
        announce an_syn_readRsp, input;
        input_readRsp = cast_syn_readRsp(input);
        __y3 = input_readRsp.va;
        __y4 = input_readRsp.st;
      }};
      assert true;
      receive { case syn_writeRsp: (input: tsyn_writeRsp) {
        announce an_syn_writeRsp, input;
        input_writeRsp = cast_syn_writeRsp(input);
        __y5 = input_writeRsp.va;
      }};
      assert true;
    }

  }
}

