machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_int: set[int], domain_bool: set[bool], domain_tKey: set[int], domain_tNode: set[tNode])) {
      var setting: setting;
      var domain_int: set[int];
      var domain_bool: set[bool];
      var domain_tKey: set[int];
      var domain_tNode: set[tNode];
      var k: tKey;
      var x: int;
      var input_writeToMid: (key: tKey, va: int, node: tNode);
      var tmp_18: tKey;
      var tmp_19: int;
      var n_7: tNode;
      var tmp_16: tKey;
      var tmp_17: int;
      var n_4: tNode;
      var y: int;
      var tmp_13: tKey;
      var tmp_14: int;
      var tmp_15: tNode;
      var tmp_10: tKey;
      var tmp_11: int;
      var tmp_12: tNode;
      var input_writeToTail: (key: tKey, va: int);
      var tmp_8: tKey;
      var tmp_9: int;
      var tmp_6: tKey;
      var tmp_7: int;
      var input_writeRsp: (key: tKey, va: int);
      var tmp_4: tKey;
      var tmp_5: int;
      var input_readRsp: (key: tKey, va: int, st: bool);
      var tmp_2: tKey;
      var tmp_3: int;
      var s_0: bool;
      var tmp_0: tKey;
      var tmp_1: int;
      setting = input.setting;
      domain_int = input.domain_int;
      domain_bool = input.domain_bool;
      domain_tKey = input.domain_tKey;
      domain_tNode = input.domain_tNode;
      while(true){
        k = choose(domain_tKey);
        x = choose(domain_int);
        if (true) {
          break;
        };
      };
      send_writeReq(this, setting, (key = k, va = x));
      receive { case syn_writeToMid: (input: tsyn_writeToMid) {
        forward_syn_writeToMid(input);
        input_writeToMid = cast_syn_writeToMid(input);
        tmp_18 = input_writeToMid.key;
        tmp_19 = input_writeToMid.va;
        n_7 = input_writeToMid.node;
      }};
      assert ((((tmp_18 == k) && (tmp_19 == x)) && (n_7 == Node1)) && !((n_7 == Node2)));
      receive { case syn_writeToMid: (input: tsyn_writeToMid) {
        forward_syn_writeToMid(input);
        input_writeToMid = cast_syn_writeToMid(input);
        tmp_16 = input_writeToMid.key;
        tmp_17 = input_writeToMid.va;
        n_4 = input_writeToMid.node;
      }};
      assert (((((tmp_16 == k) && (tmp_17 == x)) && !((n_7 == n_4))) && !((n_4 == Node1))) && (n_4 == Node2));
      while(true){
        y = choose(domain_int);
        if (!((y == x))) {
          break;
        };
      };
      send_writeReq(this, setting, (key = k, va = y));
      receive { case syn_writeToMid: (input: tsyn_writeToMid) {
        forward_syn_writeToMid(input);
        input_writeToMid = cast_syn_writeToMid(input);
        tmp_13 = input_writeToMid.key;
        tmp_14 = input_writeToMid.va;
        tmp_15 = input_writeToMid.node;
      }};
      assert (((tmp_13 == k) && (tmp_14 == y)) && (tmp_15 == n_7));
      receive { case syn_writeToMid: (input: tsyn_writeToMid) {
        forward_syn_writeToMid(input);
        input_writeToMid = cast_syn_writeToMid(input);
        tmp_10 = input_writeToMid.key;
        tmp_11 = input_writeToMid.va;
        tmp_12 = input_writeToMid.node;
      }};
      assert (((tmp_10 == k) && (tmp_11 == y)) && (tmp_12 == n_4));
      receive { case syn_writeToTail: (input: tsyn_writeToTail) {
        forward_syn_writeToTail(input);
        input_writeToTail = cast_syn_writeToTail(input);
        tmp_8 = input_writeToTail.key;
        tmp_9 = input_writeToTail.va;
      }};
      assert ((tmp_8 == k) && (tmp_9 == y));
      receive { case syn_writeToTail: (input: tsyn_writeToTail) {
        forward_syn_writeToTail(input);
        input_writeToTail = cast_syn_writeToTail(input);
        tmp_6 = input_writeToTail.key;
        tmp_7 = input_writeToTail.va;
      }};
      assert ((tmp_6 == k) && (tmp_7 == x));
      send_crashTail(this, setting);
      receive { case syn_writeRsp: (input: tsyn_writeRsp) {
        input_writeRsp = cast_syn_writeRsp(input);
        tmp_4 = input_writeRsp.key;
        tmp_5 = input_writeRsp.va;
      }};
      assert ((tmp_4 == k) && (tmp_5 == x));
      send_readReq(this, setting, (key = k,));
      receive { case syn_readRsp: (input: tsyn_readRsp) {
        input_readRsp = cast_syn_readRsp(input);
        tmp_2 = input_readRsp.key;
        tmp_3 = input_readRsp.va;
        s_0 = input_readRsp.st;
      }};
      assert (((tmp_2 == k) && (tmp_3 == y)) && s_0);
      receive { case syn_writeRsp: (input: tsyn_writeRsp) {
        input_writeRsp = cast_syn_writeRsp(input);
        tmp_0 = input_writeRsp.key;
        tmp_1 = input_writeRsp.va;
      }};
      assert ((tmp_0 == k) && (tmp_1 == y));
    }

  }
}

