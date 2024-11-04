machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_int: set[int])) {
      var setting: setting;
      var domain_int: set[int];
      var input_ePing: (trial: int);
      var tl_43: int;
      var input_ePong: (trial: int);
      var tmp_5: int;
      var input_ePongLost: (trial: int);
      var tmp_4: int;
      var tl_23: int;
      var tmp_3: int;
      var tmp_2: int;
      var tl_3: int;
      var tmp_1: int;
      var tmp_0: int;
      setting = input.setting;
      domain_int = input.domain_int;
      send_eStart(this, setting);
      receive { case syn_ePing: (input: tsyn_ePing) {
        forward_syn_ePing(input);
        input_ePing = cast_syn_ePing(input);
        tl_43 = input_ePing.trial;
      }};
      assert (((tl_43 == 1) && !((tl_43 == 2))) && !((tl_43 == 3)));
      receive { case syn_ePong: (input: tsyn_ePong) {
        forward_syn_ePong(input);
        input_ePong = cast_syn_ePong(input);
        tmp_5 = input_ePong.trial;
      }};
      assert (tmp_5 == tl_43);
      send_eNetworkError(this, setting, (trial = tl_43,));
      receive { case syn_ePongLost: (input: tsyn_ePongLost) {
        forward_syn_ePongLost(input);
        input_ePongLost = cast_syn_ePongLost(input);
        tmp_4 = input_ePongLost.trial;
      }};
      assert (tmp_4 == tl_43);
      receive { case syn_ePing: (input: tsyn_ePing) {
        forward_syn_ePing(input);
        input_ePing = cast_syn_ePing(input);
        tl_23 = input_ePing.trial;
      }};
      assert (((!((tl_43 == tl_23)) && !((tl_23 == 1))) && (tl_23 == 2)) && !((tl_23 == 3)));
      receive { case syn_ePong: (input: tsyn_ePong) {
        forward_syn_ePong(input);
        input_ePong = cast_syn_ePong(input);
        tmp_3 = input_ePong.trial;
      }};
      assert (tmp_3 == tl_23);
      send_eNetworkError(this, setting, (trial = tl_23,));
      receive { case syn_ePongLost: (input: tsyn_ePongLost) {
        forward_syn_ePongLost(input);
        input_ePongLost = cast_syn_ePongLost(input);
        tmp_2 = input_ePongLost.trial;
      }};
      assert (tmp_2 == tl_23);
      receive { case syn_ePing: (input: tsyn_ePing) {
        forward_syn_ePing(input);
        input_ePing = cast_syn_ePing(input);
        tl_3 = input_ePing.trial;
      }};
      assert ((((!((tl_43 == tl_3)) && !((tl_3 == tl_23))) && !((tl_3 == 1))) && !((tl_3 == 2))) && (tl_3 == 3));
      receive { case syn_ePong: (input: tsyn_ePong) {
        forward_syn_ePong(input);
        input_ePong = cast_syn_ePong(input);
        tmp_1 = input_ePong.trial;
      }};
      assert (tmp_1 == tl_3);
      send_eNetworkError(this, setting, (trial = tl_3,));
      receive { case syn_ePongLost: (input: tsyn_ePongLost) {
        forward_syn_ePongLost(input);
        input_ePongLost = cast_syn_ePongLost(input);
        tmp_0 = input_ePongLost.trial;
      }};
      assert (tmp_0 == tl_3);
      receive { case syn_eNotifyNodesDown: (input: tsyn_eNotifyNodesDown) {
        break;
      }};
      assert true;
    }

  }
}

