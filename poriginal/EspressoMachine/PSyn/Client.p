  
  machine Client {
    var setting: setting;
    var domain_tCoffeeMakerState: set[tCoffeeMakerState];
    var domain_action: set[randimAction];
    var counter: int;
    var timer: Timer;
    start state Syn {
      entry (input: (setting: setting, domain_tCoffeeMakerState: set[tCoffeeMakerState])) {
        setting = input.setting;
        domain_tCoffeeMakerState = input.domain_tCoffeeMakerState;
        domain_action += (Start);
        domain_action += (User);
        domain_action += (DoNothing);
        send_eCoffeeMachineUser(this, setting);
      }
  
      on syn_eCoffeeMakerReady do {
        send_eEspressoButtonPressed (this, setting);
      }

      ignore syn_eCoffeeMakerError;
    }
  }