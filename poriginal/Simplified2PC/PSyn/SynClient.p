enum randimAction {
  Write,
  Read,
  DoNothing
}

machine SynClient {
  var timer: Timer;
  var setting: setting;
  var domain_int: set[int];
  var domain_bool: set[bool];
  var domain_action: set[randimAction];
  var counter: int;
  var node: machine;
  fun doAction () {
    var ra: randimAction;
    counter = counter + 1;
    ra = choose(domain_action);
    if (counter > 5) {
      raise halt;
    }
    if (ra == Write) {
      send_writeReq(this, setting, (va = choose(domain_int),));
    } else if (ra == Read) {
      send_readReq(this, setting);
    } else {
      return;
    }
  }
  start state Syn {
    entry (input: (setting: setting, domain_int: set[int], domain_bool: set[bool])) {
      counter = 0;
      timer = CreateTimer(this);
      setting = input.setting;
      domain_int = input.domain_int;
      domain_action += (Write);
      domain_action += (Read);
      domain_action += (DoNothing);
      StartTimer(timer);
    }

    on eTimeOut do {
      CancelTimer(timer);
      doAction ();
      StartTimer(timer);
    }
    ignore syn_readRsp, syn_writeRsp;
  }
}
