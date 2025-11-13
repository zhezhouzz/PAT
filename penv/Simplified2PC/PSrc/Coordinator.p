machine Coordinator
{
  var database: machine;
  var user: machine;
  var purRsp_buffer: tsyn_putRsp;
  var is_committed: bool;

  start state WaitForRequests {
    entry (input:(database: machine)){
        database = input.database;
    }

    on syn_writeReq do (input: tsyn_writeReq) {
        user = input.controller;
        send input.controller, syn_putReq, (controller = input.controller, dst = database, src = this, va = input.va);
    }

    on syn_readReq do (input: tsyn_readReq) {
        send input.controller, syn_getReq, (controller = input.controller, dst = database);
    }

    on syn_putRsp do (input: tsyn_putRsp) {
        // print format("input: {0}", input);
        purRsp_buffer = input;
        if (input.stat) {
            send input.controller, syn_commit, (controller = input.controller, dst = database);
            send user, syn_writeRsp, (controller = user, dst = user, va = purRsp_buffer.va, stat = purRsp_buffer.stat);
        } else {
            send input.controller, syn_abort, (controller = input.controller, dst = database);
            send input.controller, syn_writeRsp, (controller = input.controller, dst = input.controller, va = input.va, stat = input.stat);
        }
    }

    // on syn_putRsp do (input: tsyn_putRsp) {
	    //     // print format("input: {0}", input);
    //     purRsp_buffer = input;
    //     if (input.stat) {
	    //         if ($) {
	    //             is_committed = true;
    //             send input.controller, syn_commit, (controller = input.controller, dst = database);
    //         } else {
	    //             is_committed = false;
    //             send input.controller, syn_writeRsp, (controller = input.controller, dst = input.controller, va = input.va, stat = input.stat);
    //         }
    //         StartTimer(timer);
    //     } else {
	    //         send input.controller, syn_abort, (controller = input.controller, dst = database);
    //         send input.controller, syn_writeRsp, (controller = input.controller, dst = input.controller, va = input.va, stat = input.stat);
    //     }
    // }

    // on eTimeOut do {
	    //     if (is_committed) {
	    //         send user, syn_writeRsp, (controller = user, dst = user, va = purRsp_buffer.va, stat = purRsp_buffer.stat);
    //     } else {
	    //         send user, syn_commit, (controller = user, dst = database);
    //     }
    // }
  }
}


