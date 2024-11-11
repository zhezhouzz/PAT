enum randimAction {
  Wirte,
  Read,
  DoNothing
}

machine SynClient {
  var setting: setting;
  var domain_int: set[int];
  var domain_bool: set[bool];
  var domain_action: set[randimAction];
  var counter: int;
  var timer: Timer;
  fun doAction () {
    var ra: randimAction;
    ra = choose(domain_action);
    counter = counter + 1;
    if (counter > 5) {
      raise halt;
    }
    if (ra == Wirte) {
      RealSend(setting, syn_writeReq, (controller = this, dst = setting, va = choose(domain_int)));
    } else if (ra == Read) {
      RealSend(setting, syn_readReq, (controller = this, dst = setting));
    } else {
      return;
    }
  }
  start state Syn {
    entry (input: (setting: setting, domain_int: set[int], domain_bool: set[bool])) {
      setting = input.setting;
      domain_int = input.domain_int;
      domain_bool = input.domain_bool;
      counter = 0;
      domain_action += (Wirte);
      domain_action += (Read);
      domain_action += (DoNothing);
      timer = CreateTimer(this);
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

