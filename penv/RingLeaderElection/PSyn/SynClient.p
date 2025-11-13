machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_tNode: set[tNode])) {
      var setting: setting;
      var domain_tNode: set[tNode];
      var __x0: tNode;
      var input_eNominate: (node: tNode, leader: tNode);
      var __y1: tNode;
      var __y2: tNode;
      var __y3: tNode;
      var __y4: tNode;
      var __x5: tNode;
      var input_eWon: (leader: tNode);
      var __y6: tNode;
      var __y7: tNode;
      var __y8: tNode;
      var __y9: tNode;
      var __y10: tNode;
      var __y11: tNode;
      setting = input.setting;
      domain_tNode = input.domain_tNode;
      while(true){
        __x0 = choose(domain_tNode);
        if (true) {
          break;
        };
      };
      send_eWakeup(this, setting, (node = __x0,));
      receive { case syn_eNominate: (input: tsyn_eNominate) {
        announce an_syn_eNominate, input;
        forward_syn_eNominate(input);
        input_eNominate = cast_syn_eNominate(input);
        __y1 = input_eNominate.node;
        __y2 = input_eNominate.leader;
      }};
      assert true;
      receive { case syn_eNominate: (input: tsyn_eNominate) {
        announce an_syn_eNominate, input;
        forward_syn_eNominate(input);
        input_eNominate = cast_syn_eNominate(input);
        __y3 = input_eNominate.node;
        __y4 = input_eNominate.leader;
      }};
      assert true;
      while(true){
        __x5 = choose(domain_tNode);
        if (!((__x5 == __x0))) {
          break;
        };
      };
      send_eWakeup(this, setting, (node = __x5,));
      receive { case syn_eWon: (input: tsyn_eWon) {
        announce an_syn_eWon, input;
        input_eWon = cast_syn_eWon(input);
        __y6 = input_eWon.leader;
      }};
      assert true;
      receive { case syn_eNominate: (input: tsyn_eNominate) {
        announce an_syn_eNominate, input;
        forward_syn_eNominate(input);
        input_eNominate = cast_syn_eNominate(input);
        __y7 = input_eNominate.node;
        __y8 = input_eNominate.leader;
      }};
      assert true;
      receive { case syn_eNominate: (input: tsyn_eNominate) {
        announce an_syn_eNominate, input;
        forward_syn_eNominate(input);
        input_eNominate = cast_syn_eNominate(input);
        __y9 = input_eNominate.node;
        __y10 = input_eNominate.leader;
      }};
      assert true;
      receive { case syn_eWon: (input: tsyn_eWon) {
        announce an_syn_eWon, input;
        input_eWon = cast_syn_eWon(input);
        __y11 = input_eWon.leader;
      }};
      assert true;
    }

  }
}

