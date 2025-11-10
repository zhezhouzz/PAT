machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_int: set[int])) {
      var setting: setting;
      var domain_int: set[int];
      var input_ePing: (trial: int);
      var __y0: int;
      var input_ePong: (trial: int);
      var __y1: int;
      var input_ePongLost: (trial: int);
      var __y3: int;
      var __y4: int;
      var __y5: int;
      var __y7: int;
      var __y8: int;
      var __y9: int;
      var __y11: int;
      setting = input.setting;
      domain_int = input.domain_int;
      send_eStart(this, setting);
      receive { case syn_ePing: (input: tsyn_ePing) {
        announce an_syn_ePing, input;
        forward_syn_ePing(input);
        input_ePing = cast_syn_ePing(input);
        __y0 = input_ePing.trial;
      }};
      assert true;
      receive { case syn_ePong: (input: tsyn_ePong) {
        announce an_syn_ePong, input;
        forward_syn_ePong(input);
        input_ePong = cast_syn_ePong(input);
        __y1 = input_ePong.trial;
      }};
      assert true;
      send_eNetworkError(this, setting, (trial = __y0,));
      receive { case syn_ePongLost: (input: tsyn_ePongLost) {
        announce an_syn_ePongLost, input;
        forward_syn_ePongLost(input);
        input_ePongLost = cast_syn_ePongLost(input);
        __y3 = input_ePongLost.trial;
      }};
      assert true;
      receive { case syn_ePing: (input: tsyn_ePing) {
        announce an_syn_ePing, input;
        forward_syn_ePing(input);
        input_ePing = cast_syn_ePing(input);
        __y4 = input_ePing.trial;
      }};
      assert true;
      receive { case syn_ePong: (input: tsyn_ePong) {
        announce an_syn_ePong, input;
        forward_syn_ePong(input);
        input_ePong = cast_syn_ePong(input);
        __y5 = input_ePong.trial;
      }};
      assert true;
      send_eNetworkError(this, setting, (trial = __y4,));
      receive { case syn_ePongLost: (input: tsyn_ePongLost) {
        announce an_syn_ePongLost, input;
        forward_syn_ePongLost(input);
        input_ePongLost = cast_syn_ePongLost(input);
        __y7 = input_ePongLost.trial;
      }};
      assert true;
      receive { case syn_ePing: (input: tsyn_ePing) {
        announce an_syn_ePing, input;
        forward_syn_ePing(input);
        input_ePing = cast_syn_ePing(input);
        __y8 = input_ePing.trial;
      }};
      assert true;
      receive { case syn_ePong: (input: tsyn_ePong) {
        announce an_syn_ePong, input;
        forward_syn_ePong(input);
        input_ePong = cast_syn_ePong(input);
        __y9 = input_ePong.trial;
      }};
      assert true;
      send_eNetworkError(this, setting, (trial = __y8,));
      receive { case syn_ePongLost: (input: tsyn_ePongLost) {
        announce an_syn_ePongLost, input;
        forward_syn_ePongLost(input);
        input_ePongLost = cast_syn_ePongLost(input);
        __y11 = input_ePongLost.trial;
      }};
      assert true;
      receive { case syn_eNotifyNodesDown: (input: tsyn_eNotifyNodesDown) {
        announce an_syn_eNotifyNodesDown, input;
        break;
      }};
      assert true;
    }

  }
}

