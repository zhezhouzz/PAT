machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_tNode: set[tNode])) {
      var setting: setting;
      var domain_tNode: set[tNode];
      var n_8: tNode;
      var ld: tNode;
      var input_eNominate: (node: tNode, leader: tNode);
      var tmp_8: tNode;
      var tmp_9: tNode;
      var tmp_6: tNode;
      var tmp_7: tNode;
      var input_eWon: (leader: tNode);
      var tmp_5: tNode;
      var tmp_3: tNode;
      var tmp_4: tNode;
      var tmp_1: tNode;
      var tmp_2: tNode;
      var tmp_0: tNode;
      setting = input.setting;
      domain_tNode = input.domain_tNode;
      while(true){
        n_8 = choose(domain_tNode);
        if (((n_8 == Node1) && !((n_8 == Node2)))) {
          break;
        };
      };
      send_eWakeup(this, setting, (node = n_8,));
      while(true){
        ld = choose(domain_tNode);
        if (((!((n_8 == ld)) && !((ld == Node1))) && (ld == Node2))) {
          break;
        };
      };
      send_eWakeup(this, setting, (node = ld,));
      receive { case syn_eNominate: (input: tsyn_eNominate) {
        forward_syn_eNominate(input);
        input_eNominate = cast_syn_eNominate(input);
        tmp_8 = input_eNominate.node;
        tmp_9 = input_eNominate.leader;
      }};
      assert ((tmp_8 == ld) && (tmp_9 == ld));
      receive { case syn_eNominate: (input: tsyn_eNominate) {
        forward_syn_eNominate(input);
        input_eNominate = cast_syn_eNominate(input);
        tmp_6 = input_eNominate.node;
        tmp_7 = input_eNominate.leader;
      }};
      assert ((tmp_6 == n_8) && (tmp_7 == ld));
      receive { case syn_eWon: (input: tsyn_eWon) {
        input_eWon = cast_syn_eWon(input);
        tmp_5 = input_eWon.leader;
      }};
      assert (tmp_5 == ld);
      receive { case syn_eNominate: (input: tsyn_eNominate) {
        forward_syn_eNominate(input);
        input_eNominate = cast_syn_eNominate(input);
        tmp_3 = input_eNominate.node;
        tmp_4 = input_eNominate.leader;
      }};
      assert ((tmp_3 == n_8) && (tmp_4 == n_8));
      receive { case syn_eNominate: (input: tsyn_eNominate) {
        forward_syn_eNominate(input);
        input_eNominate = cast_syn_eNominate(input);
        tmp_1 = input_eNominate.node;
        tmp_2 = input_eNominate.leader;
      }};
      assert ((tmp_1 == ld) && (tmp_2 == n_8));
      receive { case syn_eWon: (input: tsyn_eWon) {
        input_eWon = cast_syn_eWon(input);
        tmp_0 = input_eWon.leader;
      }};
      assert (tmp_0 == n_8);
    }

  }
}

