enum randimAction {
  Init,
  Withdraw,
  DoNothing
}

machine SynClient {
  var setting: setting;
  var domain_int: set[int];
  var domain_bool: set[bool];
  var domain_aid: set[int];
  var domain_rid: set[int];
  var domain_action: set[randimAction];
  var timer: Timer;
  var counter: int;
  var node: machine;
  fun doAction () {
    var ra: randimAction;
    counter = counter + 1;
    ra = choose(domain_action);
    if (counter > 5) {
      raise halt;
    }
    if (ra == Init) {
      send_eInitAccount(this, setting, (accountId = choose(domain_aid), balance = choose(domain_int)));
    } else if (ra == Withdraw) {
      send_eWithDrawReq(this, setting, (rId = choose(domain_rid), accountId = choose(domain_aid), amount = choose(domain_int)));
    } else {
      return;
    }
  }
  start state Syn {
    entry (input: (setting: setting, domain_int: set[int], domain_bool: set[bool], domain_aid: set[int], domain_rid: set[int])) {
      setting = input.setting;
      domain_int = input.domain_int;
      domain_bool = input.domain_bool;
      domain_aid = input.domain_aid;
      domain_rid = input.domain_rid;
      counter = 0;
      timer = CreateTimer(this);
      domain_action += (Init);
      domain_action += (Withdraw);
      domain_action += (DoNothing);
      StartTimer(timer);
    }

    on eTimeOut do {
      CancelTimer(timer);
      doAction ();
      StartTimer(timer);
    }
    ignore syn_eWithDrawResp;
  }
}