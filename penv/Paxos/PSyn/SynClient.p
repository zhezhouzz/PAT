machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_tVal: set[int], domain_tAcceptorNode: set[tAcceptorNode], domain_tProposerNode: set[tProposerNode])) {
      var setting: setting;
      var domain_tVal: set[int];
      var domain_tAcceptorNode: set[tAcceptorNode];
      var domain_tProposerNode: set[tProposerNode];
      var p_77: tProposerNode;
      var ac_10: tAcceptorNode;
      var ap_2: tProposerNode;
      var ac_65: tAcceptorNode;
      var y: tVal;
      var input_ePrepareReq: (proposer: tProposerNode, acceptor: tAcceptorNode, va: tVal);
      var tmp_25: tProposerNode;
      var tmp_26: tAcceptorNode;
      var tmp_27: tVal;
      var input_ePrepareRsp: (acceptor: tAcceptorNode, promised: tProposerNode, va: tVal, n_accepted: tProposerNode);
      var tmp_22: tAcceptorNode;
      var tmp_23: tProposerNode;
      var tmp_24: tVal;
      var ap_7: tProposerNode;
      var x: tVal;
      var tmp_19: tProposerNode;
      var tmp_20: tAcceptorNode;
      var tmp_21: tVal;
      var input_eAcceptReq: (proposer: tProposerNode, acceptor: tAcceptorNode, va: tVal);
      var tmp_16: tProposerNode;
      var tmp_17: tAcceptorNode;
      var tmp_18: tVal;
      var input_eAcceptRsp: (proposer: tProposerNode, acceptor: tAcceptorNode, accepted: tProposerNode, va: tVal);
      var tmp_12: tProposerNode;
      var tmp_13: tAcceptorNode;
      var tmp_14: tProposerNode;
      var tmp_15: tVal;
      var tmp_9: tAcceptorNode;
      var tmp_10: tProposerNode;
      var tmp_11: tVal;
      var ap_22: tProposerNode;
      var tmp_6: tProposerNode;
      var tmp_7: tAcceptorNode;
      var tmp_8: tVal;
      var tmp_2: tProposerNode;
      var tmp_3: tAcceptorNode;
      var tmp_4: tProposerNode;
      var tmp_5: tVal;
      var input_eLearn: (va: tVal);
      var tmp_1: tVal;
      var tmp_0: tVal;
      setting = input.setting;
      domain_tVal = input.domain_tVal;
      domain_tAcceptorNode = input.domain_tAcceptorNode;
      domain_tProposerNode = input.domain_tProposerNode;
      while(true){
        p_77 = choose(domain_tProposerNode);
        ac_10 = choose(domain_tAcceptorNode);
        if (((ac_10 == Acceptor2) && !((ac_10 == Acceptor1)))) {
          break;
        };
      };
      send_eLostPrepareReq(this, setting, (proposer = p_77, acceptor = ac_10));
      while(true){
        ap_2 = choose(domain_tProposerNode);
        ac_65 = choose(domain_tAcceptorNode);
        if ((((!((ac_65 == ac_10)) && !((ac_65 == Acceptor2))) && (ac_65 == Acceptor1)) && !((p_77 == ap_2)))) {
          break;
        };
      };
      send_eLostPrepareReq(this, setting, (proposer = ap_2, acceptor = ac_65));
      while(true){
        y = choose(domain_tVal);
        if (true) {
          break;
        };
      };
      send_eStart(this, setting, (proposer = ap_2, va = y));
      receive { case syn_ePrepareReq: (input: tsyn_ePrepareReq) {
        forward_syn_ePrepareReq(input);
        input_ePrepareReq = cast_syn_ePrepareReq(input);
        tmp_25 = input_ePrepareReq.proposer;
        tmp_26 = input_ePrepareReq.acceptor;
        tmp_27 = input_ePrepareReq.va;
      }};
      assert (((tmp_25 == ap_2) && (tmp_26 == ac_10)) && (tmp_27 == y));
      receive { case syn_ePrepareRsp: (input: tsyn_ePrepareRsp) {
        forward_syn_ePrepareRsp(input);
        input_ePrepareRsp = cast_syn_ePrepareRsp(input);
        tmp_22 = input_ePrepareRsp.acceptor;
        tmp_23 = input_ePrepareRsp.promised;
        tmp_24 = input_ePrepareRsp.va;
        ap_7 = input_ePrepareRsp.n_accepted;
      }};
      assert ((((tmp_22 == ac_10) && (tmp_23 == ap_2)) && (tmp_24 == y)) && (((p_77 == ap_7) && !((ap_7 == ap_2))) || (!((p_77 == ap_7)) && (ap_7 == ap_2))));
      while(true){
        x = choose(domain_tVal);
        if (!((y == x))) {
          break;
        };
      };
      send_eStart(this, setting, (proposer = p_77, va = x));
      receive { case syn_ePrepareReq: (input: tsyn_ePrepareReq) {
        forward_syn_ePrepareReq(input);
        input_ePrepareReq = cast_syn_ePrepareReq(input);
        tmp_19 = input_ePrepareReq.proposer;
        tmp_20 = input_ePrepareReq.acceptor;
        tmp_21 = input_ePrepareReq.va;
      }};
      assert (((tmp_19 == p_77) && (tmp_20 == ac_65)) && (tmp_21 == x));
      receive { case syn_eAcceptReq: (input: tsyn_eAcceptReq) {
        forward_syn_eAcceptReq(input);
        input_eAcceptReq = cast_syn_eAcceptReq(input);
        tmp_16 = input_eAcceptReq.proposer;
        tmp_17 = input_eAcceptReq.acceptor;
        tmp_18 = input_eAcceptReq.va;
      }};
      assert (((tmp_16 == ap_2) && (tmp_17 == ac_10)) && (tmp_18 == y));
      receive { case syn_eAcceptRsp: (input: tsyn_eAcceptRsp) {
        forward_syn_eAcceptRsp(input);
        input_eAcceptRsp = cast_syn_eAcceptRsp(input);
        tmp_12 = input_eAcceptRsp.proposer;
        tmp_13 = input_eAcceptRsp.acceptor;
        tmp_14 = input_eAcceptRsp.accepted;
        tmp_15 = input_eAcceptRsp.va;
      }};
      assert ((((tmp_12 == ap_2) && (tmp_13 == ac_10)) && (tmp_14 == ap_2)) && (tmp_15 == y));
      receive { case syn_ePrepareRsp: (input: tsyn_ePrepareRsp) {
        forward_syn_ePrepareRsp(input);
        input_ePrepareRsp = cast_syn_ePrepareRsp(input);
        tmp_9 = input_ePrepareRsp.acceptor;
        tmp_10 = input_ePrepareRsp.promised;
        tmp_11 = input_ePrepareRsp.va;
        ap_22 = input_ePrepareRsp.n_accepted;
      }};
      assert ((((tmp_9 == ac_65) && (tmp_10 == p_77)) && (tmp_11 == x)) && ((((((p_77 == ap_22) && !((ap_7 == ap_22))) && !((ap_22 == ap_2))) || (((p_77 == ap_22) && (ap_7 == ap_22)) && !((ap_22 == ap_2)))) || ((!((p_77 == ap_22)) && !((ap_7 == ap_22))) && (ap_22 == ap_2))) || ((!((p_77 == ap_22)) && (ap_7 == ap_22)) && (ap_22 == ap_2))));
      receive { case syn_eAcceptReq: (input: tsyn_eAcceptReq) {
        forward_syn_eAcceptReq(input);
        input_eAcceptReq = cast_syn_eAcceptReq(input);
        tmp_6 = input_eAcceptReq.proposer;
        tmp_7 = input_eAcceptReq.acceptor;
        tmp_8 = input_eAcceptReq.va;
      }};
      assert (((tmp_6 == p_77) && (tmp_7 == ac_65)) && (tmp_8 == x));
      receive { case syn_eAcceptRsp: (input: tsyn_eAcceptRsp) {
        forward_syn_eAcceptRsp(input);
        input_eAcceptRsp = cast_syn_eAcceptRsp(input);
        tmp_2 = input_eAcceptRsp.proposer;
        tmp_3 = input_eAcceptRsp.acceptor;
        tmp_4 = input_eAcceptRsp.accepted;
        tmp_5 = input_eAcceptRsp.va;
      }};
      assert ((((tmp_2 == p_77) && (tmp_3 == ac_65)) && (tmp_4 == p_77)) && (tmp_5 == x));
      receive { case syn_eLearn: (input: tsyn_eLearn) {
        forward_syn_eLearn(input);
        input_eLearn = cast_syn_eLearn(input);
        tmp_1 = input_eLearn.va;
      }};
      assert (tmp_1 == x);
      receive { case syn_eLearn: (input: tsyn_eLearn) {
        forward_syn_eLearn(input);
        input_eLearn = cast_syn_eLearn(input);
        tmp_0 = input_eLearn.va;
      }};
      assert (tmp_0 == y);
    }

  }
}

