machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_bool: set[bool], domain_tNode: set[tNode])) {
      var setting: setting;
      var domain_bool: set[bool];
      var domain_tNode: set[tNode];
      var n: tNode;
      var input_eInternalReq: (node: tNode);
      var tmp_7: tNode;
      var n_25: tNode;
      var tmp_6: tNode;
      var input_eForwardReq: (node: tNode);
      var tmp_5: tNode;
      var input_eExternalReq: (node: tNode);
      var tmp_4: tNode;
      var input_eExternalRsp: (node: tNode, stat: bool);
      var tmp_3: tNode;
      var st_5: bool;
      var tmp_2: tNode;
      var tmp_1: tNode;
      var tmp_0: tNode;
      var st_0: bool;
      setting = input.setting;
      domain_bool = input.domain_bool;
      domain_tNode = input.domain_tNode;
      while(true){
        n = choose(domain_tNode);
        if (true) {
          break;
        };
      };
      send_eStart(this, setting, (node = n,));
      receive { case syn_eInternalReq: (input: tsyn_eInternalReq) {
        forward_syn_eInternalReq(input);
        input_eInternalReq = cast_syn_eInternalReq(input);
        tmp_7 = input_eInternalReq.node;
      }};
      assert (tmp_7 == n);
      while(true){
        n_25 = choose(domain_tNode);
        if (!((n_25 == n))) {
          break;
        };
      };
      send_eStart(this, setting, (node = n_25,));
      receive { case syn_eInternalReq: (input: tsyn_eInternalReq) {
        forward_syn_eInternalReq(input);
        input_eInternalReq = cast_syn_eInternalReq(input);
        tmp_6 = input_eInternalReq.node;
      }};
      assert (tmp_6 == n_25);
      receive { case syn_eForwardReq: (input: tsyn_eForwardReq) {
        forward_syn_eForwardReq(input);
        input_eForwardReq = cast_syn_eForwardReq(input);
        tmp_5 = input_eForwardReq.node;
      }};
      assert (tmp_5 == n_25);
      receive { case syn_eExternalReq: (input: tsyn_eExternalReq) {
        forward_syn_eExternalReq(input);
        input_eExternalReq = cast_syn_eExternalReq(input);
        tmp_4 = input_eExternalReq.node;
      }};
      assert (tmp_4 == n_25);
      receive { case syn_eExternalRsp: (input: tsyn_eExternalRsp) {
        forward_syn_eExternalRsp(input);
        input_eExternalRsp = cast_syn_eExternalRsp(input);
        tmp_3 = input_eExternalRsp.node;
        st_5 = input_eExternalRsp.stat;
      }};
      assert ((tmp_3 == n_25) && st_5);
      receive { case syn_eForwardReq: (input: tsyn_eForwardReq) {
        forward_syn_eForwardReq(input);
        input_eForwardReq = cast_syn_eForwardReq(input);
        tmp_2 = input_eForwardReq.node;
      }};
      assert (tmp_2 == n);
      receive { case syn_eExternalReq: (input: tsyn_eExternalReq) {
        forward_syn_eExternalReq(input);
        input_eExternalReq = cast_syn_eExternalReq(input);
        tmp_1 = input_eExternalReq.node;
      }};
      assert (tmp_1 == n);
      receive { case syn_eExternalRsp: (input: tsyn_eExternalRsp) {
        forward_syn_eExternalRsp(input);
        input_eExternalRsp = cast_syn_eExternalRsp(input);
        tmp_0 = input_eExternalRsp.node;
        st_0 = input_eExternalRsp.stat;
      }};
      assert ((tmp_0 == n) && !(st_0));
    }

  }
}

