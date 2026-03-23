machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_tAcceptorNode: set[tAcceptorNode], domain_tProposerNode: set[tProposerNode], domain_tVal: set[tVal])) {
      var setting: setting;
      var domain_tAcceptorNode: set[tAcceptorNode];
      var domain_tProposerNode: set[tProposerNode];
      var domain_tVal: set[tVal];
      var __x0: tProposerNode;
      var __x1: tAcceptorNode;
      var __x2: tProposerNode;
      var __x3: tVal;
      var input_ePrepareReq: (proposer: tProposerNode, acceptor: tAcceptorNode, va: tVal);
      var __y4: tProposerNode;
      var __y5: tAcceptorNode;
      var __y6: tVal;
      var input_ePrepareRsp: (acceptor: tAcceptorNode, promised: tProposerNode, va: tVal, n_accepted: tProposerNode);
      var __y7: tAcceptorNode;
      var __y8: tProposerNode;
      var __y9: tVal;
      var __y10: tProposerNode;
      var input_eAcceptReq: (proposer: tProposerNode, acceptor: tAcceptorNode, va: tVal);
      var __y11: tProposerNode;
      var __y12: tAcceptorNode;
      var __y13: tVal;
      var input_eAcceptRsp: (proposer: tProposerNode, acceptor: tAcceptorNode, accepted: tProposerNode, va: tVal);
      var __y14: tProposerNode;
      var __y15: tAcceptorNode;
      var __y16: tProposerNode;
      var __y17: tVal;
      var input_eLearn: (va: tVal);
      var __y18: tVal;
      var __y19: tProposerNode;
      var __y20: tAcceptorNode;
      var __y21: tVal;
      var __y22: tAcceptorNode;
      var __y23: tProposerNode;
      var __y24: tVal;
      var __y25: tProposerNode;
      var __y26: tProposerNode;
      var __y27: tAcceptorNode;
      var __y28: tVal;
      var __y29: tProposerNode;
      var __y30: tAcceptorNode;
      var __y31: tProposerNode;
      var __y32: tVal;
      var __y33: tVal;
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
        __x2 = choose(domain_tProposerNode);
        __x3 = choose(domain_tVal);
        if (true) {
          break;
        };
      };
      send_eStart(this, setting, (proposer = __x2, va = __x3));
      receive { case syn_ePrepareReq: (input: tsyn_ePrepareReq) {
        announce an_syn_ePrepareReq, input;
        forward_syn_ePrepareReq(input);
        input_ePrepareReq = cast_syn_ePrepareReq(input);
        __y4 = input_ePrepareReq.proposer;
        __y5 = input_ePrepareReq.acceptor;
        __y6 = input_ePrepareReq.va;
      }};
      assert true;
      receive { case syn_ePrepareRsp: (input: tsyn_ePrepareRsp) {
        announce an_syn_ePrepareRsp, input;
        forward_syn_ePrepareRsp(input);
        input_ePrepareRsp = cast_syn_ePrepareRsp(input);
        __y7 = input_ePrepareRsp.acceptor;
        __y8 = input_ePrepareRsp.promised;
        __y9 = input_ePrepareRsp.va;
        __y10 = input_ePrepareRsp.n_accepted;
      }};
      assert true;
      receive { case syn_eAcceptReq: (input: tsyn_eAcceptReq) {
        announce an_syn_eAcceptReq, input;
        forward_syn_eAcceptReq(input);
        input_eAcceptReq = cast_syn_eAcceptReq(input);
        __y11 = input_eAcceptReq.proposer;
        __y12 = input_eAcceptReq.acceptor;
        __y13 = input_eAcceptReq.va;
      }};
      assert true;
      receive { case syn_eAcceptRsp: (input: tsyn_eAcceptRsp) {
        announce an_syn_eAcceptRsp, input;
        forward_syn_eAcceptRsp(input);
        input_eAcceptRsp = cast_syn_eAcceptRsp(input);
        __y14 = input_eAcceptRsp.proposer;
        __y15 = input_eAcceptRsp.acceptor;
        __y16 = input_eAcceptRsp.accepted;
        __y17 = input_eAcceptRsp.va;
      }};
      assert true;
      receive { case syn_eLearn: (input: tsyn_eLearn) {
        announce an_syn_eLearn, input;
        forward_syn_eLearn(input);
        input_eLearn = cast_syn_eLearn(input);
        __y18 = input_eLearn.va;
      }};
      assert true;
      receive { case syn_ePrepareReq: (input: tsyn_ePrepareReq) {
        announce an_syn_ePrepareReq, input;
        forward_syn_ePrepareReq(input);
        input_ePrepareReq = cast_syn_ePrepareReq(input);
        __y19 = input_ePrepareReq.proposer;
        __y20 = input_ePrepareReq.acceptor;
        __y21 = input_ePrepareReq.va;
      }};
      assert true;
      receive { case syn_ePrepareRsp: (input: tsyn_ePrepareRsp) {
        announce an_syn_ePrepareRsp, input;
        forward_syn_ePrepareRsp(input);
        input_ePrepareRsp = cast_syn_ePrepareRsp(input);
        __y22 = input_ePrepareRsp.acceptor;
        __y23 = input_ePrepareRsp.promised;
        __y24 = input_ePrepareRsp.va;
        __y25 = input_ePrepareRsp.n_accepted;
      }};
      assert true;
      receive { case syn_eAcceptReq: (input: tsyn_eAcceptReq) {
        announce an_syn_eAcceptReq, input;
        forward_syn_eAcceptReq(input);
        input_eAcceptReq = cast_syn_eAcceptReq(input);
        __y26 = input_eAcceptReq.proposer;
        __y27 = input_eAcceptReq.acceptor;
        __y28 = input_eAcceptReq.va;
      }};
      assert true;
      receive { case syn_eAcceptRsp: (input: tsyn_eAcceptRsp) {
        announce an_syn_eAcceptRsp, input;
        forward_syn_eAcceptRsp(input);
        input_eAcceptRsp = cast_syn_eAcceptRsp(input);
        __y29 = input_eAcceptRsp.proposer;
        __y30 = input_eAcceptRsp.acceptor;
        __y31 = input_eAcceptRsp.accepted;
        __y32 = input_eAcceptRsp.va;
      }};
      assert true;
      receive { case syn_eLearn: (input: tsyn_eLearn) {
        announce an_syn_eLearn, input;
        forward_syn_eLearn(input);
        input_eLearn = cast_syn_eLearn(input);
        __y33 = input_eLearn.va;
      }};
      assert true;
    }

  }
}

