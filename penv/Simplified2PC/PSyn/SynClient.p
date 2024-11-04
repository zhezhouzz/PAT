machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_int: set[int], domain_bool: set[bool])) {
      var setting: setting;
      var domain_int: set[int];
      var domain_bool: set[bool];
      var x: int;
      var input_putReq: (va: int);
      var tmp_3: int;
      var input_putRsp: (va: int, stat: bool);
      var tmp_2: int;
      var s_6: bool;
      var input_writeRsp: (va: int, stat: bool);
      var tmp_0: int;
      var tmp_1: bool;
      var input_readRsp: (va: int);
      var y: int;
      setting = input.setting;
      domain_int = input.domain_int;
      domain_bool = input.domain_bool;
      while(true){
        x = choose(domain_int);
        if (!((x == -1))) {
          break;
        };
      };
      send_writeReq(this, setting, (va = x,));
      receive { case syn_putReq: (input: tsyn_putReq) {
        forward_syn_putReq(input);
        input_putReq = cast_syn_putReq(input);
        tmp_3 = input_putReq.va;
      }};
      assert (tmp_3 == x);
      receive { case syn_putRsp: (input: tsyn_putRsp) {
        forward_syn_putRsp(input);
        input_putRsp = cast_syn_putRsp(input);
        tmp_2 = input_putRsp.va;
        s_6 = input_putRsp.stat;
      }};
      assert ((tmp_2 == x) && s_6);
      receive { case syn_writeRsp: (input: tsyn_writeRsp) {
        input_writeRsp = cast_syn_writeRsp(input);
        tmp_0 = input_writeRsp.va;
        tmp_1 = input_writeRsp.stat;
      }};
      assert ((tmp_0 == x) && (tmp_1 == s_6));
      send_readReq(this, setting);
      receive { case syn_getReq: (input: tsyn_getReq) {
        forward_syn_getReq(input);
        break;
      }};
      assert true;
      receive { case syn_readRsp: (input: tsyn_readRsp) {
        input_readRsp = cast_syn_readRsp(input);
        y = input_readRsp.va;
      }};
      assert (!((y == x)) && (y == -1));
      receive { case syn_commit: (input: tsyn_commit) {
        forward_syn_commit(input);
        break;
      }};
      assert true;
    }

  }
}

