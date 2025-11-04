machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_tAcceptorNode: set[tAcceptorNode], domain_tProposerNode: set[tProposerNode], domain_tVal: set[tVal])) {
      var setting: setting;
      var domain_tAcceptorNode: set[tAcceptorNode];
      var domain_tProposerNode: set[tProposerNode];
      var domain_tVal: set[tVal];
      var __x0: tProposerNode;
      var __x1: tAcceptorNode;
      var __x2: tVal;
      var input_ePrepareReq: (proposer: tProposerNode, acceptor: tAcceptorNode, va: tVal);
      var __y3: tProposerNode;
      var __y4: tAcceptorNode;
      var __y5: tVal;
      var input_ePrepareRsp: (acceptor: tAcceptorNode, promised: tProposerNode, va: tVal, n_accepted: tProposerNode);
      var __y6: tAcceptorNode;
      var __y7: tProposerNode;
      var __y8: tVal;
      var __y9: tProposerNode;
      var __x10: tProposerNode;
      var __x11: tAcceptorNode;
      var __x12: tVal;
      var __y13: tProposerNode;
      var __y14: tAcceptorNode;
      var __y15: tVal;
      var input_eAcceptReq: (proposer: tProposerNode, acceptor: tAcceptorNode, va: tVal);
      var __y16: tProposerNode;
      var __y17: tAcceptorNode;
      var __y18: tVal;
      var input_eAcceptRsp: (proposer: tProposerNode, acceptor: tAcceptorNode, accepted: tProposerNode, va: tVal);
      var __y19: tProposerNode;
      var __y20: tAcceptorNode;
      var __y21: tProposerNode;
      var __y22: tVal;
      var __y23: tAcceptorNode;
      var __y24: tProposerNode;
      var __y25: tVal;
      var __y26: tProposerNode;
      var __y27: tProposerNode;
      var __y28: tAcceptorNode;
      var __y29: tVal;
      var __y30: tProposerNode;
      var __y31: tAcceptorNode;
      var __y32: tProposerNode;
      var __y33: tVal;
      var input_eLearn: (va: tVal);
      var __y34: tVal;
      var __y35: tVal;
      setting = input.setting;
      domain_tAcceptorNode = input.domain_tAcceptorNode;
      domain_tProposerNode = input.domain_tProposerNode;
      domain_tVal = input.domain_tVal;
      while(true){
        __x0 = choose(domain_tProposerNode);
        __x1 = choose(domain_tAcceptorNode);
        if ((acc2(__x1) && pr1(__x0))) {
          break;
        };
      };
      send_eLostPrepareReq(this, setting, (proposer = __x0, acceptor = __x1));
      while(true){
        __x2 = choose(domain_tVal);
        if (true) {
          break;
        };
      };
      send_eStart(this, setting, (proposer = __x0, va = __x2));
      receive { case syn_ePrepareReq: (input: tsyn_ePrepareReq) {
        announce an_syn_ePrepareReq, input;
        forward_syn_ePrepareReq(input);
        input_ePrepareReq = cast_syn_ePrepareReq(input);
        __y3 = input_ePrepareReq.proposer;
        __y4 = input_ePrepareReq.acceptor;
        __y5 = input_ePrepareReq.va;
      }};
      assert true;
      receive { case syn_ePrepareRsp: (input: tsyn_ePrepareRsp) {
        announce an_syn_ePrepareRsp, input;
        forward_syn_ePrepareRsp(input);
        input_ePrepareRsp = cast_syn_ePrepareRsp(input);
        __y6 = input_ePrepareRsp.acceptor;
        __y7 = input_ePrepareRsp.promised;
        __y8 = input_ePrepareRsp.va;
        __y9 = input_ePrepareRsp.n_accepted;
      }};
      assert true;
      while(true){
        __x10 = choose(domain_tProposerNode);
        __x11 = choose(domain_tAcceptorNode);
        if (((acc2(__x11) && !((__x0 == __x10))) && pr2(__x10))) {
          break;
        };
      };
      send_eLostPrepareReq(this, setting, (proposer = __x10, acceptor = __x11));
      while(true){
        __x12 = choose(domain_tVal);
        if (!((__x2 == __x12))) {
          break;
        };
      };
      send_eStart(this, setting, (proposer = __x10, va = __x12));
      receive { case syn_ePrepareReq: (input: tsyn_ePrepareReq) {
        announce an_syn_ePrepareReq, input;
        forward_syn_ePrepareReq(input);
        input_ePrepareReq = cast_syn_ePrepareReq(input);
        __y13 = input_ePrepareReq.proposer;
        __y14 = input_ePrepareReq.acceptor;
        __y15 = input_ePrepareReq.va;
      }};
      assert true;
      receive { case syn_eAcceptReq: (input: tsyn_eAcceptReq) {
        announce an_syn_eAcceptReq, input;
        forward_syn_eAcceptReq(input);
        input_eAcceptReq = cast_syn_eAcceptReq(input);
        __y16 = input_eAcceptReq.proposer;
        __y17 = input_eAcceptReq.acceptor;
        __y18 = input_eAcceptReq.va;
      }};
      assert true;
      receive { case syn_eAcceptRsp: (input: tsyn_eAcceptRsp) {
        announce an_syn_eAcceptRsp, input;
        forward_syn_eAcceptRsp(input);
        input_eAcceptRsp = cast_syn_eAcceptRsp(input);
        __y19 = input_eAcceptRsp.proposer;
        __y20 = input_eAcceptRsp.acceptor;
        __y21 = input_eAcceptRsp.accepted;
        __y22 = input_eAcceptRsp.va;
      }};
      assert true;
      receive { case syn_ePrepareRsp: (input: tsyn_ePrepareRsp) {
        announce an_syn_ePrepareRsp, input;
        forward_syn_ePrepareRsp(input);
        input_ePrepareRsp = cast_syn_ePrepareRsp(input);
        __y23 = input_ePrepareRsp.acceptor;
        __y24 = input_ePrepareRsp.promised;
        __y25 = input_ePrepareRsp.va;
        __y26 = input_ePrepareRsp.n_accepted;
      }};
      assert true;
      receive { case syn_eAcceptReq: (input: tsyn_eAcceptReq) {
        announce an_syn_eAcceptReq, input;
        forward_syn_eAcceptReq(input);
        input_eAcceptReq = cast_syn_eAcceptReq(input);
        __y27 = input_eAcceptReq.proposer;
        __y28 = input_eAcceptReq.acceptor;
        __y29 = input_eAcceptReq.va;
      }};
      assert true;
      receive { case syn_eAcceptRsp: (input: tsyn_eAcceptRsp) {
        announce an_syn_eAcceptRsp, input;
        forward_syn_eAcceptRsp(input);
        input_eAcceptRsp = cast_syn_eAcceptRsp(input);
        __y30 = input_eAcceptRsp.proposer;
        __y31 = input_eAcceptRsp.acceptor;
        __y32 = input_eAcceptRsp.accepted;
        __y33 = input_eAcceptRsp.va;
      }};
      assert true;
      receive { case syn_eLearn: (input: tsyn_eLearn) {
        announce an_syn_eLearn, input;
        forward_syn_eLearn(input);
        input_eLearn = cast_syn_eLearn(input);
        __y34 = input_eLearn.va;
      }};
      assert true;
      receive { case syn_eLearn: (input: tsyn_eLearn) {
        announce an_syn_eLearn, input;
        forward_syn_eLearn(input);
        input_eLearn = cast_syn_eLearn(input);
        __y35 = input_eLearn.va;
      }};
      assert true;
    }

  }
}

