enum randimAction {
  Start,
  DoNothing
}

machine SynClient {
  var timer: Timer;
  var setting: setting;
  var domain_bool: set[bool];
  var domain_tNode: set[tNode];
  var domain_action: set[randimAction];
  var counter: int;
  var node: machine;
  fun doAction () {
    var ra: randimAction;
    counter = counter + 1;
    ra = choose(domain_action);
    if (counter > 2) {
      raise halt;
    }
    if (ra == Start) {
      send_eStart(this, setting, (node = choose(domain_tNode),));
    } else {
      return;
    }
  }
  start state Syn {
    entry (input: (setting: setting, domain_bool: set[bool], domain_tNode: set[tNode])) {
      setting = input.setting;
      domain_bool = input.domain_bool;
      domain_tNode = input.domain_tNode;
      counter = 0;
      timer = CreateTimer(this);
      domain_action += (Start);
      domain_action += (DoNothing);
      StartTimer(timer);
    }

    on eTimeOut do {
      CancelTimer(timer);
      doAction ();
      StartTimer(timer);
    }
  }
}
