machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_bool: set[bool], domain_tNode: set[tNode], domain_tVal: set[tVal])) {
      var setting: setting;
      var domain_bool: set[bool];
      var domain_tNode: set[tNode];
      var domain_tVal: set[tVal];
      var __x0: tVal;
      var input_eAppendEntry: (node: tNode, va: tVal);
      var __y1: tNode;
      var __y2: tVal;
      var input_eTimeout: (dest: tNode);
      var __y3: tNode;
      var input_eVoteReq: (src: tNode, dest: tNode, leader: tNode);
      var __y4: tNode;
      var __y5: tNode;
      var __y6: tNode;
      var input_eVoteRsp: (src: tNode, dest: tNode, stat: bool);
      var __y7: tNode;
      var __y8: tNode;
      var __y9: bool;
      var input_eBecomeLeader: (leader: tNode);
      var __y10: tNode;
      var __y11: tNode;
      var __y12: tNode;
      var __y13: tNode;
      var __y14: tNode;
      var __y15: tNode;
      var __y16: tNode;
      var __y17: bool;
      var __y18: tNode;
      var __y19: tNode;
      var __y20: tVal;
      setting = input.setting;
      domain_bool = input.domain_bool;
      domain_tNode = input.domain_tNode;
      domain_tVal = input.domain_tVal;
      while(true){
        __x0 = choose(domain_tVal);
        if (true) {
          break;
        };
      };
      send_eClientPut(this, setting, (va = __x0,));
      receive { case syn_eAppendEntry: (input: tsyn_eAppendEntry) {
        announce an_syn_eAppendEntry, input;
        forward_syn_eAppendEntry(input);
        input_eAppendEntry = cast_syn_eAppendEntry(input);
        __y1 = input_eAppendEntry.node;
        __y2 = input_eAppendEntry.va;
      }};
      assert true;
      send_eShutDown(this, setting);
      receive { case syn_eTimeout: (input: tsyn_eTimeout) {
        announce an_syn_eTimeout, input;
        forward_syn_eTimeout(input);
        input_eTimeout = cast_syn_eTimeout(input);
        __y3 = input_eTimeout.dest;
      }};
      assert true;
      receive { case syn_eVoteReq: (input: tsyn_eVoteReq) {
        announce an_syn_eVoteReq, input;
        forward_syn_eVoteReq(input);
        input_eVoteReq = cast_syn_eVoteReq(input);
        __y4 = input_eVoteReq.src;
        __y5 = input_eVoteReq.dest;
        __y6 = input_eVoteReq.leader;
      }};
      assert true;
      receive { case syn_eVoteRsp: (input: tsyn_eVoteRsp) {
        announce an_syn_eVoteRsp, input;
        forward_syn_eVoteRsp(input);
        input_eVoteRsp = cast_syn_eVoteRsp(input);
        __y7 = input_eVoteRsp.src;
        __y8 = input_eVoteRsp.dest;
        __y9 = input_eVoteRsp.stat;
      }};
      assert true;
      receive { case syn_eBecomeLeader: (input: tsyn_eBecomeLeader) {
        announce an_syn_eBecomeLeader, input;
        forward_syn_eBecomeLeader(input);
        input_eBecomeLeader = cast_syn_eBecomeLeader(input);
        __y10 = input_eBecomeLeader.leader;
      }};
      assert true;
      receive { case syn_eTimeout: (input: tsyn_eTimeout) {
        announce an_syn_eTimeout, input;
        forward_syn_eTimeout(input);
        input_eTimeout = cast_syn_eTimeout(input);
        __y11 = input_eTimeout.dest;
      }};
      assert true;
      receive { case syn_eVoteReq: (input: tsyn_eVoteReq) {
        announce an_syn_eVoteReq, input;
        forward_syn_eVoteReq(input);
        input_eVoteReq = cast_syn_eVoteReq(input);
        __y12 = input_eVoteReq.src;
        __y13 = input_eVoteReq.dest;
        __y14 = input_eVoteReq.leader;
      }};
      assert true;
      receive { case syn_eVoteRsp: (input: tsyn_eVoteRsp) {
        announce an_syn_eVoteRsp, input;
        forward_syn_eVoteRsp(input);
        input_eVoteRsp = cast_syn_eVoteRsp(input);
        __y15 = input_eVoteRsp.src;
        __y16 = input_eVoteRsp.dest;
        __y17 = input_eVoteRsp.stat;
      }};
      assert true;
      receive { case syn_eBecomeLeader: (input: tsyn_eBecomeLeader) {
        announce an_syn_eBecomeLeader, input;
        forward_syn_eBecomeLeader(input);
        input_eBecomeLeader = cast_syn_eBecomeLeader(input);
        __y18 = input_eBecomeLeader.leader;
      }};
      assert true;
      receive { case syn_eAppendEntry: (input: tsyn_eAppendEntry) {
        announce an_syn_eAppendEntry, input;
        forward_syn_eAppendEntry(input);
        input_eAppendEntry = cast_syn_eAppendEntry(input);
        __y19 = input_eAppendEntry.node;
        __y20 = input_eAppendEntry.va;
      }};
      assert true;
    }

  }
}

