machine FailureDetector {
  var timer: Timer;
  var node: machine;
  var tl: int;
  var controller: machine;

  start state Init {
    entry (input: (node: machine)) {
      node = input.node;
      timer = CreateTimer(this);
      StartTimer(timer);
      receive { 
        case eStart: (input: teStart) {
          CancelTimer(timer);
          controller = input.controller;
          tl = 1;
          RealSend(node, ePing, (controller = input.controller, dst = node, fd = this, trial = tl));
          StartTimer(timer);
          goto Wait;
        }
        case eTimeOut: {
          goto End;
        }
      }
    }
  }

  state Wait {
    entry{}
    on ePong do (input: tePong) {
      goto End;
    }
    on eTimeOut do {
      CancelTimer(timer);
      if(tl == 2) {
        RealSend(controller, eNotifyNodesDown, (controller = controller, dst = controller));
      }
      tl = tl + 1;
      RealSend(node, ePing, (controller = controller, dst = node, fd = this, trial = tl));
      StartTimer(timer);
    }
  }

  state End {
    entry {}
    ignore eStart, eTimeOut, ePong;
  }
}