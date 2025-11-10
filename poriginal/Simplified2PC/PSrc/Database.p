machine Database
{
  var store: int;
  var buffer: int;

  start state WaitForRequests {
    entry {
      store = -1;
    }

    on syn_putReq do (input: tsyn_putReq) {
      var res: (va: int, stat: bool); 
      if($){
        buffer = input.va;
        res.va = input.va;
        res.stat = true;   
      } else {
        res.va = input.va;
        res.stat = false;
      }
      RealSend(input.src, syn_putRsp, (controller = input.controller, dst = input.src, va = res.va, stat = res.stat));
    }

    on syn_getReq do (input: tsyn_getReq) {
      var res: (va: int);
      res.va = store; 
      RealSend(input.controller, syn_readRsp, (controller = input.controller, dst = input.controller, va = res.va));
    }

    on syn_commit do {
      store = buffer;
      buffer = -1;
    }

    on syn_abort do {
      buffer = -1;
    }
  }
}


