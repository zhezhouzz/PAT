machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_bool: set[bool], domain_tNode: set[tNode])) {
      var setting: setting;
      var domain_bool: set[bool];
      var domain_tNode: set[tNode];
      var __x0: tNode;
      var input_eInternalReq: (node: tNode);
      var __y1: tNode;
      var input_eForwardReq: (node: tNode);
      var __y2: tNode;
      var input_eExternalReq: (node: tNode);
      var __y3: tNode;
      var input_eExternalRsp: (node: tNode, stat: bool);
      var __y4: tNode;
      var __y5: bool;
      var __y6: tNode;
      var __y7: bool;
      setting = input.setting;
      domain_bool = input.domain_bool;
      domain_tNode = input.domain_tNode;
      while(true){
        __x0 = choose(domain_tNode);
        if (true) {
          break;
        };
      };
      send_eStart(this, setting, (node = __x0,));
      receive { case syn_eInternalReq: (input: tsyn_eInternalReq) {
        announce an_syn_eInternalReq, input;
        forward_syn_eInternalReq(input);
        input_eInternalReq = cast_syn_eInternalReq(input);
        __y1 = input_eInternalReq.node;
      }};
      assert true;
      receive { case syn_eForwardReq: (input: tsyn_eForwardReq) {
        announce an_syn_eForwardReq, input;
        forward_syn_eForwardReq(input);
        input_eForwardReq = cast_syn_eForwardReq(input);
        __y2 = input_eForwardReq.node;
      }};
      assert true;
      receive { case syn_eExternalReq: (input: tsyn_eExternalReq) {
        announce an_syn_eExternalReq, input;
        forward_syn_eExternalReq(input);
        input_eExternalReq = cast_syn_eExternalReq(input);
        __y3 = input_eExternalReq.node;
      }};
      assert true;
      receive { case syn_eExternalRsp: (input: tsyn_eExternalRsp) {
        announce an_syn_eExternalRsp, input;
        forward_syn_eExternalRsp(input);
        input_eExternalRsp = cast_syn_eExternalRsp(input);
        __y4 = input_eExternalRsp.node;
        __y5 = input_eExternalRsp.stat;
      }};
      assert true;
      receive { case syn_eExternalRsp: (input: tsyn_eExternalRsp) {
        announce an_syn_eExternalRsp, input;
        forward_syn_eExternalRsp(input);
        input_eExternalRsp = cast_syn_eExternalRsp(input);
        __y6 = input_eExternalRsp.node;
        __y7 = input_eExternalRsp.stat;
      }};
      assert true;
    }

  }
}

