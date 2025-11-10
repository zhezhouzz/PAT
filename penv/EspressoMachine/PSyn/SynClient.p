machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_tCoffeeMakerState: set[tCoffeeMakerState])) {
      var setting: setting;
      var domain_tCoffeeMakerState: set[tCoffeeMakerState];
      var input_eCoffeeMakerError: (st: tCoffeeMakerState);
      var __y0: tCoffeeMakerState;
      setting = input.setting;
      domain_tCoffeeMakerState = input.domain_tCoffeeMakerState;
      send_eCoffeeMachineUser(this, setting);
      receive { case syn_eWarmUpReq: (input: tsyn_eWarmUpReq) {
        announce an_syn_eWarmUpReq, input;
        forward_syn_eWarmUpReq(input);
        break;
      }};
      assert true;
      receive { case syn_eWarmUpCompleted: (input: tsyn_eWarmUpCompleted) {
        announce an_syn_eWarmUpCompleted, input;
        forward_syn_eWarmUpCompleted(input);
        break;
      }};
      assert true;
      receive { case syn_eCoffeeMakerReady: (input: tsyn_eCoffeeMakerReady) {
        announce an_syn_eCoffeeMakerReady, input;
        break;
      }};
      assert true;
      send_eEspressoButtonPressed(this, setting);
      receive { case syn_eGrindBeansReq: (input: tsyn_eGrindBeansReq) {
        announce an_syn_eGrindBeansReq, input;
        forward_syn_eGrindBeansReq(input);
        break;
      }};
      assert true;
      receive { case syn_eGrindBeansCompleted: (input: tsyn_eGrindBeansCompleted) {
        announce an_syn_eGrindBeansCompleted, input;
        forward_syn_eGrindBeansCompleted(input);
        break;
      }};
      assert true;
      receive { case syn_eStartEspressoReq: (input: tsyn_eStartEspressoReq) {
        announce an_syn_eStartEspressoReq, input;
        forward_syn_eStartEspressoReq(input);
        break;
      }};
      assert true;
      receive { case syn_eNoWaterError: (input: tsyn_eNoWaterError) {
        announce an_syn_eNoWaterError, input;
        forward_syn_eNoWaterError(input);
        break;
      }};
      assert true;
      receive { case syn_eCoffeeMakerError: (input: tsyn_eCoffeeMakerError) {
        announce an_syn_eCoffeeMakerError, input;
        input_eCoffeeMakerError = cast_syn_eCoffeeMakerError(input);
        __y0 = input_eCoffeeMakerError.st;
      }};
      assert true;
    }

  }
}

