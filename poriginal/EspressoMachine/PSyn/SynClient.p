enum randimAction {
  Start,
  User,
  DoNothing
}

machine SynClient {
  var setting: setting;
  var domain_tCoffeeMakerState: set[tCoffeeMakerState];
  var domain_action: set[randimAction];
  var counter: int;
  var timer: Timer;
  fun doAction () {
    var ra: randimAction;
    counter = counter + 1;
    ra = choose(domain_action);
    if (counter > 2) {
      raise halt;
    }
    if (ra == Start) {
      send_eEspressoButtonPressed (this, setting);
    } else if (ra == User) {
      send_eCoffeeMachineUser(this, setting);
    } else {
      send_eCoffeeMachineUser(this, setting);
      // return;
    }
  }
  start state Syn {
    entry (input: (setting: setting, domain_tCoffeeMakerState: set[tCoffeeMakerState])) {
      setting = input.setting;
      domain_tCoffeeMakerState = input.domain_tCoffeeMakerState;
      domain_action += (Start);
      domain_action += (User);
      domain_action += (DoNothing);
      timer = CreateTimer(this);
      StartTimer(timer);
    }

    on eTimeOut do {
      CancelTimer(timer);
      doAction ();
      StartTimer(timer);
    }
    ignore syn_eCoffeeMakerReady, syn_eCoffeeMakerError;
  }
}