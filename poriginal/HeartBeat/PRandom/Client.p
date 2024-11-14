
  machine Client {
    var timer: Timer;
    var setting: setting;
    var domain_int: set[int];
    var domain_action: set[randimAction];
    var counter: int;
    var node: machine;
    var is_shut_down: bool;
    start state Syn {
      entry (input: (node: machine, setting: setting, domain_int: set[int])) {
        counter = 0;
        timer = CreateTimer(this);
        node = input.node;
        setting = input.setting;
        domain_int = input.domain_int;
        domain_action += (Start);
        domain_action += (ShutDown);
        domain_action += (DoNothing);
        RealSend(setting, eStart, (controller = this, dst = setting));
        is_shut_down = false;
        StartTimer(timer);
      }
  
      on eTimeOut do {
        CancelTimer(timer);
        if(!is_shut_down && $) {
            is_shut_down = true;
            RealSend(node, eShutDown, node);
        } else {
            StartTimer(timer);
        }
      }
      on eNotifyNodesDown do {
        raise halt;
      }
    }
  }
  
  