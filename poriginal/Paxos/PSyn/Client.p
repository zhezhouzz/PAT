
  
  machine Client {
    var timer: Timer;
    var setting: setting;
    var domain_tVal: set[int];
    var domain_tAcceptorNode: set[tAcceptorNode];
    var domain_tProposerNode: set[tProposerNode];
    var domain_action: set[randimAction];
    var counter: int;
    var node: machine;
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
        send_eStart(this, setting, (proposer = Proposer1, va = 1));
        send_eStart(this, setting, (proposer = Proposer2, va = 2));
      }
    }
  }
  
  