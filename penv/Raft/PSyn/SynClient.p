machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_bool: set[bool], domain_tVal: set[int], domain_tNode: set[tNode])) {
      var setting: setting;
      var domain_bool: set[bool];
      var domain_tVal: set[int];
      var domain_tNode: set[tNode];
      var x: tVal;
      var input_eAppendEntry: (node: tNode, va: tVal);
      var n1: tNode;
      var tmp_22: tVal;
      var input_eTimeout: (dest: tNode);
      var tmp_21: tNode;
      var input_eVoteReq: (src: tNode, dest: tNode, leader: tNode);
      var tmp_19: tNode;
      var n2: tNode;
      var tmp_20: tNode;
      var input_eVoteRsp: (src: tNode, dest: tNode, stat: bool);
      var tmp_17: tNode;
      var tmp_18: tNode;
      var st_21: bool;
      var tmp_16: tNode;
      var tmp_15: tNode;
      var tmp_12: tNode;
      var tmp_13: tNode;
      var tmp_14: tNode;
      var tmp_10: tNode;
      var tmp_11: tNode;
      var st_8: bool;
      var tmp_9: tNode;
      var tmp_6: tNode;
      var tmp_7: tNode;
      var tmp_8: tNode;
      var tmp_4: tNode;
      var tmp_5: tNode;
      var st_1: bool;
      var input_eBecomeLeader: (leader: tNode);
      var tmp_3: tNode;
      var tmp_2: tNode;
      var tmp_0: tNode;
      var tmp_1: tVal;
      setting = input.setting;
      domain_bool = input.domain_bool;
      domain_tVal = input.domain_tVal;
      domain_tNode = input.domain_tNode;
      while(true){
        x = choose(domain_tVal);
        if (true) {
          break;
        };
      };
      send_eClientPut(this, setting, (va = x,));
      receive { case syn_eAppendEntry: (input: tsyn_eAppendEntry) {
        forward_syn_eAppendEntry(input);
        input_eAppendEntry = cast_syn_eAppendEntry(input);
        n1 = input_eAppendEntry.node;
        tmp_22 = input_eAppendEntry.va;
      }};
      assert (((tmp_22 == x) && !((n1 == Node1))) && (n1 == Node2));
      send_eShutDown(this, setting);
      receive { case syn_eTimeout: (input: tsyn_eTimeout) {
        forward_syn_eTimeout(input);
        input_eTimeout = cast_syn_eTimeout(input);
        tmp_21 = input_eTimeout.dest;
      }};
      assert (tmp_21 == n1);
      receive { case syn_eVoteReq: (input: tsyn_eVoteReq) {
        forward_syn_eVoteReq(input);
        input_eVoteReq = cast_syn_eVoteReq(input);
        tmp_19 = input_eVoteReq.src;
        n2 = input_eVoteReq.dest;
        tmp_20 = input_eVoteReq.leader;
      }};
      assert (((((tmp_19 == n1) && (tmp_20 == n1)) && !((n2 == n1))) && (n2 == Node1)) && !((n2 == Node2)));
      receive { case syn_eVoteRsp: (input: tsyn_eVoteRsp) {
        forward_syn_eVoteRsp(input);
        input_eVoteRsp = cast_syn_eVoteRsp(input);
        tmp_17 = input_eVoteRsp.src;
        tmp_18 = input_eVoteRsp.dest;
        st_21 = input_eVoteRsp.stat;
      }};
      assert (((tmp_17 == n2) && (tmp_18 == n1)) && st_21);
      receive { case syn_eTimeout: (input: tsyn_eTimeout) {
        forward_syn_eTimeout(input);
        input_eTimeout = cast_syn_eTimeout(input);
        tmp_16 = input_eTimeout.dest;
      }};
      assert (tmp_16 == n2);
      send_eShutDown(this, setting);
      receive { case syn_eTimeout: (input: tsyn_eTimeout) {
        forward_syn_eTimeout(input);
        input_eTimeout = cast_syn_eTimeout(input);
        tmp_15 = input_eTimeout.dest;
      }};
      assert (tmp_15 == n1);
      receive { case syn_eVoteReq: (input: tsyn_eVoteReq) {
        forward_syn_eVoteReq(input);
        input_eVoteReq = cast_syn_eVoteReq(input);
        tmp_12 = input_eVoteReq.src;
        tmp_13 = input_eVoteReq.dest;
        tmp_14 = input_eVoteReq.leader;
      }};
      assert (((tmp_12 == n1) && (tmp_13 == n2)) && (tmp_14 == n1));
      receive { case syn_eVoteRsp: (input: tsyn_eVoteRsp) {
        forward_syn_eVoteRsp(input);
        input_eVoteRsp = cast_syn_eVoteRsp(input);
        tmp_10 = input_eVoteRsp.src;
        tmp_11 = input_eVoteRsp.dest;
        st_8 = input_eVoteRsp.stat;
      }};
      assert (((tmp_10 == n2) && (tmp_11 == n1)) && st_8);
      receive { case syn_eTimeout: (input: tsyn_eTimeout) {
        forward_syn_eTimeout(input);
        input_eTimeout = cast_syn_eTimeout(input);
        tmp_9 = input_eTimeout.dest;
      }};
      assert (tmp_9 == n2);
      receive { case syn_eVoteReq: (input: tsyn_eVoteReq) {
        forward_syn_eVoteReq(input);
        input_eVoteReq = cast_syn_eVoteReq(input);
        tmp_6 = input_eVoteReq.src;
        tmp_7 = input_eVoteReq.dest;
        tmp_8 = input_eVoteReq.leader;
      }};
      assert (((tmp_6 == n2) && (tmp_7 == n1)) && (tmp_8 == n2));
      receive { case syn_eVoteRsp: (input: tsyn_eVoteRsp) {
        forward_syn_eVoteRsp(input);
        input_eVoteRsp = cast_syn_eVoteRsp(input);
        tmp_4 = input_eVoteRsp.src;
        tmp_5 = input_eVoteRsp.dest;
        st_1 = input_eVoteRsp.stat;
      }};
      assert (((tmp_4 == n1) && (tmp_5 == n2)) && !(st_1));
      receive { case syn_eBecomeLeader: (input: tsyn_eBecomeLeader) {
        forward_syn_eBecomeLeader(input);
        input_eBecomeLeader = cast_syn_eBecomeLeader(input);
        tmp_3 = input_eBecomeLeader.leader;
      }};
      assert (tmp_3 == n1);
      receive { case syn_eBecomeLeader: (input: tsyn_eBecomeLeader) {
        forward_syn_eBecomeLeader(input);
        input_eBecomeLeader = cast_syn_eBecomeLeader(input);
        tmp_2 = input_eBecomeLeader.leader;
      }};
      assert (tmp_2 == n2);
      receive { case syn_eAppendEntry: (input: tsyn_eAppendEntry) {
        forward_syn_eAppendEntry(input);
        input_eAppendEntry = cast_syn_eAppendEntry(input);
        tmp_0 = input_eAppendEntry.node;
        tmp_1 = input_eAppendEntry.va;
      }};
      assert ((tmp_0 == n2) && (tmp_1 == x));
    }

  }
}

