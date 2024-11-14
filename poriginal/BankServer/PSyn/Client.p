  
  machine Client {
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
        domain_action += (Init);
        domain_action += (Withdraw);
        domain_action += (DoNothing);
        send_eInitAccount(this, setting, (accountId = 0, balance = 10));
        send_eWithDrawReq(this, setting, (rId = 0, accountId = 0, amount = choose(domain_int)));
      }
  
      on syn_eWithDrawResp do {
        counter = counter + 1;
        if (counter < 2) {
            send_eWithDrawReq(this, setting, (rId = 0, accountId = 0, amount = choose(domain_int)));
        }
      }
    }
  }