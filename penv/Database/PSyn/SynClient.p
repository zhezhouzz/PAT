machine SynClient {
  start state Syn {
    entry (input: (setting: machine, domain_int: set[int], domain_bool: set[bool])) {
      var setting: machine;
      var domain_int: set[int];
      var domain_bool: set[bool];
      var x: int;
      var y: int;
      var input_writeRsp: (va: int, st: bool);
      var tmp_2: int;
      var s_6: bool;
      var tmp_1: int;
      var s_3: bool;
      var input_readRsp: (va: int, st: bool);
      var tmp_0: int;
      var s_0: bool;
      setting = input.setting;
      domain_int = input.domain_int;
      domain_bool = input.domain_bool;
      while(true){
        x = choose(domain_int);
        if (true) {
          break;
        };
      };
      send_writeReq(this, setting, (va = x,));
      while(true){
        y = choose(domain_int);
        if (!((y == x))) {
          break;
        };
      };
      send_writeReq(this, setting, (va = y,));
      receive { case syn_writeRsp: (input: tsyn_writeRsp) {
        input_writeRsp = cast_syn_writeRsp(input);
        tmp_2 = input_writeRsp.va;
        s_6 = input_writeRsp.st;
      }};
      assert ((tmp_2 == y) && (!(s_6) || s_6));
      receive { case syn_writeRsp: (input: tsyn_writeRsp) {
        input_writeRsp = cast_syn_writeRsp(input);
        tmp_1 = input_writeRsp.va;
        s_3 = input_writeRsp.st;
      }};
      assert ((tmp_1 == x) && s_3);
      send_readReq(this, setting);
      receive { case syn_readRsp: (input: tsyn_readRsp) {
        input_readRsp = cast_syn_readRsp(input);
        tmp_0 = input_readRsp.va;
        s_0 = input_readRsp.st;
      }};
      assert ((tmp_0 == y) && s_0);
    }

  }
}

