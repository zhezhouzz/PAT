machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_tCoffeeMakerState: set[tCoffeeMakerState])) {
      var setting: setting;
      var domain_tCoffeeMakerState: set[tCoffeeMakerState];
      var input_eCoffeeMakerError: (st: tCoffeeMakerState);
      var x_0: tCoffeeMakerState;
      setting = input.setting;
      domain_tCoffeeMakerState = input.domain_tCoffeeMakerState;
      send_eCoffeeMachineUser(this, setting);
      receive { case syn_eWarmUpReq: (input: tsyn_eWarmUpReq) {
        forward_syn_eWarmUpReq(input);
        break;
      }};
      assert true;
      receive { case syn_eWarmUpCompleted: (input: tsyn_eWarmUpCompleted) {
        forward_syn_eWarmUpCompleted(input);
        break;
      }};
      assert true;
      receive { case syn_eCoffeeMakerReady: (input: tsyn_eCoffeeMakerReady) {
        break;
      }};
      assert true;
      send_eEspressoButtonPressed(this, setting);
      receive { case syn_eGrindBeansReq: (input: tsyn_eGrindBeansReq) {
        forward_syn_eGrindBeansReq(input);
        break;
      }};
      assert true;
      receive { case syn_eGrindBeansCompleted: (input: tsyn_eGrindBeansCompleted) {
        forward_syn_eGrindBeansCompleted(input);
        break;
      }};
      assert true;
      receive { case syn_eStartEspressoReq: (input: tsyn_eStartEspressoReq) {
        forward_syn_eStartEspressoReq(input);
        break;
      }};
      assert true;
      receive { case syn_eNoWaterError: (input: tsyn_eNoWaterError) {
        forward_syn_eNoWaterError(input);
        break;
      }};
      assert true;
      receive { case syn_eCoffeeMakerError: (input: tsyn_eCoffeeMakerError) {
        input_eCoffeeMakerError = cast_syn_eCoffeeMakerError(input);
        x_0 = input_eCoffeeMakerError.st;
      }};
      assert (x_0 == NoWaterError);
    }

  }
}

