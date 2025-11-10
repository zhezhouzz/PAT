machine Coordinator
{
  var database: machine;
  var user: machine;
  var timer: Timer;
  var purRsp_buffer: tsyn_putRsp;
  var is_committed: bool;

  start state WaitForRequests {
    entry (input:(database: machine)){
        database = input.database;
        timer = CreateTimer(this);
    }

    on syn_writeReq do (input: tsyn_writeReq) {
        user = input.controller;
        RealSend(database, syn_putReq, (controller = input.controller, dst = database, src = this, va = input.va));
    }

    on syn_readReq do (input: tsyn_readReq) {
        RealSend(database, syn_getReq, (controller = input.controller, dst = database));
    }

    on syn_putRsp do (input: tsyn_putRsp) {
        // print format("input: {0}", input);
        purRsp_buffer = input;
        if (input.stat) {
            RealSend(database, syn_commit, (controller = input.controller, dst = database));
            RealSend(user, syn_writeRsp, (controller = user, dst = user, va = purRsp_buffer.va, stat = purRsp_buffer.stat));
        } else {
            RealSend(database, syn_abort, (controller = input.controller, dst = database));
            RealSend(input.controller, syn_writeRsp, (controller = input.controller, dst = input.controller, va = input.va, stat = input.stat));
        }
    }
  }
}


