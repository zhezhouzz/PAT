machine FailureDetector {
  var timer: Timer;
  var node: machine;
  var tl: int;

  start state Init {
    entry (input: (node: machine)) {
      node = input.node;
      timer = CreateTimer(this);
      receive { 
        case eStart: (input: (controller: machine)) {
          tl = 1;
          send input.controller, inePing, (controller = input.controller, dst = node, fd = this, trial = tl);
          goto Wait;
        }
      }
    }
  }

  state Wait {
    entry{}
    on inePong do (input: tinePong) {
      StartTimer(timer); 
    }
    on inePongLost do (input: tinePongLost) {
      CancelTimer(timer);
      if(input.trial == 2) {
        send input.controller, eNotifyNodesDown;
      }
      tl = tl + 1;
      send input.controller, inePing, (controller = input.controller, dst = node, fd = this, trial = tl);
    }
    on eNetworkError do (input: teNetworkError) {
      send input.controller, inePongLost, (controller = input.controller, dst = this, trial = tl);
    }
    on eTimeOut do {

    }
  }
}