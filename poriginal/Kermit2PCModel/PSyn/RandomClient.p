type setting = machine;
enum randimAction {
    UpdateReq,
    StartTxnReq,
    ReadReq,
    CommitTxnReq,
    RollbackTxnReq,
    DoNothing
  }
  
machine SynClient {
    var timer: Timer;
    var setting: setting;
    var domain_bool: set[bool];
    var domain_tGid: set[int];
    var domain_tKey: set[int];
    var domain_tVal: set[int];
    var domain_action: set[randimAction];
    var counter: int;
    var node: machine;
    fun doAction () {
      var ra: randimAction;
      counter = counter + 1;
      ra = choose(domain_action);
      if (counter > 10) {
        raise halt;
      }
      if (ra == ReadReq) {
        RealSend(setting, eReadReq, (gid=choose(domain_tGid), key=choose(domain_tKey)));
      } else if (ra == StartTxnReq) {
        RealSend(setting, eStartTxnReq, (client = this,));
      } else if (ra == UpdateReq) {
        RealSend(setting, eUpdateReq, (gid=choose(domain_tGid), key=choose(domain_tKey), val = choose(domain_tVal)));
      } else if (ra == CommitTxnReq) {
        RealSend(setting, eCommitTxnReq, (gid=choose(domain_tGid),));
      } else if (ra == RollbackTxnReq) {
        RealSend(setting, eRollbackTxnReq, (gid=choose(domain_tGid),));
      } else {
        return;
      }
    }
    start state Syn {
        entry (input: (setting: setting, domain_bool: set[bool], domain_tGid: set[int], domain_tKey: set[int], domain_tVal: set[int], domain_tCmdStatus: set[tCmdStatus], domain_tTxnStatus: set[tTxnStatus])) {
            setting = input.setting;
            domain_bool = input.domain_bool;
            domain_tGid = input.domain_tGid;
            domain_tKey = input.domain_tKey;
            domain_tVal = input.domain_tVal;
            counter = 0;
            timer = CreateTimer(this);
            domain_action += (UpdateReq);
            domain_action += (StartTxnReq);
            domain_action += (ReadReq);
            domain_action += (CommitTxnReq);
            domain_action += (RollbackTxnReq);
            domain_action += (DoNothing);
            StartTimer(timer);
        }
  
        on eTimeOut do {
            CancelTimer(timer);
            doAction ();
            StartTimer(timer);
        }
        ignore eReadRsp, eUpdateRsp, eStartTxnRsp;
    }
  }
  
  