machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_bool: set[bool], domain_int: set[int], domain_tKey: set[tKey], domain_tNode: set[tNode])) {
      var setting: setting;
      var domain_bool: set[bool];
      var domain_int: set[int];
      var domain_tKey: set[tKey];
      var domain_tNode: set[tNode];
      var __x0: tKey;
      var __x1: int;
      var __x2: int;
      var input_writeToMid: (key: tKey, va: int, node: tNode);
      var __y3: tKey;
      var __y4: int;
      var __y5: tNode;
      var __y6: tKey;
      var __y7: int;
      var __y8: tNode;
      var input_writeToTail: (key: tKey, va: int);
      var __y9: tKey;
      var __y10: int;
      var input_readRsp: (key: tKey, va: int, st: bool);
      var __y11: tKey;
      var __y12: int;
      var __y13: bool;
      var __y14: tKey;
      var __y15: int;
      var __y16: tNode;
      var __y17: tKey;
      var __y18: int;
      var __y19: tNode;
      var __y20: tKey;
      var __y21: int;
      setting = input.setting;
      domain_bool = input.domain_bool;
      domain_int = input.domain_int;
      domain_tKey = input.domain_tKey;
      domain_tNode = input.domain_tNode;
      while(true){
        __x0 = choose(domain_tKey);
        __x1 = choose(domain_int);
        if (true) {
          break;
        };
      };
      send_writeReq(this, setting, (key = __x0, va = __x1));
      while(true){
        __x2 = choose(domain_int);
        if (!((__x1 == __x2))) {
          break;
        };
      };
      send_writeReq(this, setting, (key = __x0, va = __x2));
      receive { case syn_writeToMid: (input: tsyn_writeToMid) {
        announce an_syn_writeToMid, input;
        forward_syn_writeToMid(input);
        input_writeToMid = cast_syn_writeToMid(input);
        __y3 = input_writeToMid.key;
        __y4 = input_writeToMid.va;
        __y5 = input_writeToMid.node;
      }};
      assert true;
      receive { case syn_writeToMid: (input: tsyn_writeToMid) {
        announce an_syn_writeToMid, input;
        forward_syn_writeToMid(input);
        input_writeToMid = cast_syn_writeToMid(input);
        __y6 = input_writeToMid.key;
        __y7 = input_writeToMid.va;
        __y8 = input_writeToMid.node;
      }};
      assert true;
      send_crashTail(this, setting);
      receive { case syn_writeToTail: (input: tsyn_writeToTail) {
        announce an_syn_writeToTail, input;
        forward_syn_writeToTail(input);
        input_writeToTail = cast_syn_writeToTail(input);
        __y9 = input_writeToTail.key;
        __y10 = input_writeToTail.va;
      }};
      assert true;
      send_readReq(this, setting, (key = __x0,));
      receive { case syn_readRsp: (input: tsyn_readRsp) {
        announce an_syn_readRsp, input;
        input_readRsp = cast_syn_readRsp(input);
        __y11 = input_readRsp.key;
        __y12 = input_readRsp.va;
        __y13 = input_readRsp.st;
      }};
      assert true;
      receive { case syn_writeToMid: (input: tsyn_writeToMid) {
        announce an_syn_writeToMid, input;
        forward_syn_writeToMid(input);
        input_writeToMid = cast_syn_writeToMid(input);
        __y14 = input_writeToMid.key;
        __y15 = input_writeToMid.va;
        __y16 = input_writeToMid.node;
      }};
      assert true;
      receive { case syn_writeToMid: (input: tsyn_writeToMid) {
        announce an_syn_writeToMid, input;
        forward_syn_writeToMid(input);
        input_writeToMid = cast_syn_writeToMid(input);
        __y17 = input_writeToMid.key;
        __y18 = input_writeToMid.va;
        __y19 = input_writeToMid.node;
      }};
      assert true;
      receive { case syn_writeToTail: (input: tsyn_writeToTail) {
        announce an_syn_writeToTail, input;
        forward_syn_writeToTail(input);
        input_writeToTail = cast_syn_writeToTail(input);
        __y20 = input_writeToTail.key;
        __y21 = input_writeToTail.va;
      }};
      assert true;
    }

  }
}

