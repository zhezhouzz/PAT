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
      var input_writeToMid: (key: tKey, va: int, node: tNode);
      var __y2: tKey;
      var __y3: int;
      var __y4: tNode;
      var __y5: tKey;
      var __y6: int;
      var __y7: tNode;
      var input_writeToTail: (key: tKey, va: int);
      var __y8: tKey;
      var __y9: int;
      var input_writeRsp: (key: tKey, va: int);
      var __y10: tKey;
      var __y11: int;
      var __x12: int;
      var input_readRsp: (key: tKey, va: int, st: bool);
      var __y13: tKey;
      var __y14: int;
      var __y15: bool;
      var __y16: tKey;
      var __y17: int;
      var __y18: tNode;
      var __y19: tKey;
      var __y20: int;
      var __y21: tNode;
      var __y22: tKey;
      var __y23: int;
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
      receive { case syn_writeToMid: (input: tsyn_writeToMid) {
        announce an_syn_writeToMid, input;
        forward_syn_writeToMid(input);
        input_writeToMid = cast_syn_writeToMid(input);
        __y2 = input_writeToMid.key;
        __y3 = input_writeToMid.va;
        __y4 = input_writeToMid.node;
      }};
      assert true;
      receive { case syn_writeToMid: (input: tsyn_writeToMid) {
        announce an_syn_writeToMid, input;
        forward_syn_writeToMid(input);
        input_writeToMid = cast_syn_writeToMid(input);
        __y5 = input_writeToMid.key;
        __y6 = input_writeToMid.va;
        __y7 = input_writeToMid.node;
      }};
      assert true;
      receive { case syn_writeToTail: (input: tsyn_writeToTail) {
        announce an_syn_writeToTail, input;
        forward_syn_writeToTail(input);
        input_writeToTail = cast_syn_writeToTail(input);
        __y8 = input_writeToTail.key;
        __y9 = input_writeToTail.va;
      }};
      assert true;
      receive { case syn_writeRsp: (input: tsyn_writeRsp) {
        announce an_syn_writeRsp, input;
        input_writeRsp = cast_syn_writeRsp(input);
        __y10 = input_writeRsp.key;
        __y11 = input_writeRsp.va;
      }};
      assert true;
      while(true){
        __x12 = choose(domain_int);
        if (!((__x12 == __x1))) {
          break;
        };
      };
      send_writeReq(this, setting, (key = __x0, va = __x12));
      send_readReq(this, setting, (key = __x0,));
      receive { case syn_readRsp: (input: tsyn_readRsp) {
        announce an_syn_readRsp, input;
        input_readRsp = cast_syn_readRsp(input);
        __y13 = input_readRsp.key;
        __y14 = input_readRsp.va;
        __y15 = input_readRsp.st;
      }};
      assert true;
      receive { case syn_writeToMid: (input: tsyn_writeToMid) {
        announce an_syn_writeToMid, input;
        forward_syn_writeToMid(input);
        input_writeToMid = cast_syn_writeToMid(input);
        __y16 = input_writeToMid.key;
        __y17 = input_writeToMid.va;
        __y18 = input_writeToMid.node;
      }};
      assert true;
      receive { case syn_writeToMid: (input: tsyn_writeToMid) {
        announce an_syn_writeToMid, input;
        forward_syn_writeToMid(input);
        input_writeToMid = cast_syn_writeToMid(input);
        __y19 = input_writeToMid.key;
        __y20 = input_writeToMid.va;
        __y21 = input_writeToMid.node;
      }};
      assert true;
      send_crashTail(this, setting);
      receive { case syn_writeToTail: (input: tsyn_writeToTail) {
        announce an_syn_writeToTail, input;
        forward_syn_writeToTail(input);
        input_writeToTail = cast_syn_writeToTail(input);
        __y22 = input_writeToTail.key;
        __y23 = input_writeToTail.va;
      }};
      assert true;
    }

  }
}

