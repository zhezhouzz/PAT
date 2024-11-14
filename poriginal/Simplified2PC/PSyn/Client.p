  
  machine Client {
    var timer: Timer;
    var setting: setting;
    var domain_int: set[int];
    var domain_bool: set[bool];
    var domain_action: set[randimAction];
    var counter: int;
    var node: machine;
    fun doAction () {
      counter = counter + 1;
      if (counter > 5) {
        raise halt;
      }
      if ($) {
        send_writeReq(this, setting, (va = choose(domain_int),));
      } else{
        send_readReq(this, setting);
      }
    }
    start state Syn {
      entry (input: (setting: setting, domain_int: set[int], domain_bool: set[bool])) {
        counter = 0;
        timer = CreateTimer(this);
        setting = input.setting;
        domain_int = input.domain_int;
        domain_action += (Write);
        domain_action += (Read);
        domain_action += (DoNothing);
        doAction ();
        doAction ();
        doAction ();
        doAction ();
        doAction ();
      }
  
      on syn_readRsp do {
      }

      on syn_writeRsp do {
      }
    }
  }
  