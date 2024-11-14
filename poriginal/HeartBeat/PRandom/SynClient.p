enum randimAction {
  Start,
  ShutDown,
  DoNothing
}

machine SynClient {
  var timer: Timer;
  var setting: setting;
  var domain_int: set[int];
  var domain_action: set[randimAction];
  var counter: int;
  var node: machine;
  fun doAction () {
    var ra: randimAction;
    counter = counter + 1;
    ra = choose(domain_action);
    if (counter > 10) {
      raise halt;
    }
    if (ra == Start) {
      RealSend(setting, eStart, (controller = this, dst = setting));
    } else if (ra == ShutDown) {
      RealSend(node, eShutDown, node);
    } else {
      return;
    }
  }
  start state Syn {
    entry (input: (node: machine, setting: setting, domain_int: set[int])) {
      counter = 0;
      timer = CreateTimer(this);
      node = input.node;
      setting = input.setting;
      domain_int = input.domain_int;
      domain_action += (Start);
      domain_action += (ShutDown);
      domain_action += (DoNothing);
      StartTimer(timer);
    }

    on eTimeOut do {
      CancelTimer(timer);
      doAction ();
      StartTimer(timer);
    }
    ignore eNotifyNodesDown;
  }
}

