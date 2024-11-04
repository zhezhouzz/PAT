machine FailureDetector {
  var timer: Timer;
  var node: machine;
  var tl: int;

  start state Init {
    entry (input: (node: machine)) {
      node = input.node;
      timer = CreateTimer(this);
      receive { 
        case syn_eStart: (input: tsyn_eStart) {
          tl = 1;
          send input.controller, syn_ePing, (controller = input.controller, dst = node, fd = this, trial = tl);
          goto Wait;
        }
      }
    }
  }

  state Wait {
    entry{}
    on syn_ePong do (input: tsyn_ePong) {
      StartTimer(timer); 
    }
    on syn_ePongLost do (input: tsyn_ePongLost) {
      CancelTimer(timer);
      if(input.trial == 2) {
        send input.controller, syn_eNotifyNodesDown;
      }
      tl = tl + 1;
      send input.controller, syn_ePing, (controller = input.controller, dst = node, fd = this, trial = tl);
    }
    on syn_eNetworkError do (input: tsyn_eNetworkError) {
      send input.controller, syn_ePongLost, (controller = input.controller, dst = this, trial = tl);
    }
    on eTimeOut do {

    }
  }
}