enum randimAction {
  Start,
  DoNothing
}

machine SynClient {
  var timer: Timer;
  var setting: setting;
  var domain_tVal: set[int];
  var domain_tAcceptorNode: set[tAcceptorNode];
  var domain_tProposerNode: set[tProposerNode];
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
      send_eStart(this, setting, (proposer = choose(domain_tProposerNode), va = choose(domain_tVal)));
    } else {
      return;
    }
  }
  start state Syn {
    entry (input: (setting: setting, domain_tVal: set[int], domain_tAcceptorNode: set[tAcceptorNode], domain_tProposerNode: set[tProposerNode])) {
      setting = input.setting;
      domain_tVal = input.domain_tVal;
      domain_tAcceptorNode = input.domain_tAcceptorNode;
      domain_tProposerNode = input.domain_tProposerNode;
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

