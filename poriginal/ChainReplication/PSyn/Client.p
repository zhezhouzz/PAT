
  machine Client {
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
      counter = counter + 1;
      if (counter > 5) {
        raise halt;
      }
      if ($) {
        send_writeReq(this, setting, (key = 1, va = choose(domain_int)));
      } else {
        send_readReq(this, setting, (key = 1,));
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
        doAction();
        doAction();
        doAction();
        doAction();
        doAction();
      }

      ignore syn_readRsp, syn_writeRsp;
    }
  }
  
  
  