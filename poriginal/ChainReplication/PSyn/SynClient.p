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
  var domain_tKey: set[int];
  var domain_tNode: set[tNode];
  var domain_action: set[randimAction];
  var counter: int;
  var node: machine;
  fun doAction () {
    var ra: randimAction;
    counter = counter + 1;
    ra = choose(domain_action);
    if (counter > 3) {
      raise halt;
    }
    if (ra == Write) {
      send_writeReq(this, setting, (key = choose(domain_tKey), va = choose(domain_int)));
    } else if (ra == Read) {
      send_readReq(this, setting, (key = choose(domain_tKey),));
    } else  {
      return;
    }
  }
  start state Syn {
    entry (input: (setting: setting, domain_int: set[int], domain_bool: set[bool], domain_tKey: set[int], domain_tNode: set[tNode])) {
      setting = input.setting;
      domain_int = input.domain_int;
      domain_bool = input.domain_bool;
      domain_tKey = input.domain_tKey;
      domain_tNode = input.domain_tNode;
      counter = 0;
      timer = CreateTimer(this);
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


