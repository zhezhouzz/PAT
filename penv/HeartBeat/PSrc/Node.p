/****************************
Node machine sends a pong message on receiving a ping
*****************************/
machine Node {
  start state WaitForPing {
    on syn_ePing do (req: tsyn_ePing) {
      send req.controller, syn_ePong, (controller = req.controller, dst = req.fd, trial = req.trial);
    }

    on eShutDown do {
      raise halt;
    }
  }
}