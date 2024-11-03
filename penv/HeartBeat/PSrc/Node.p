/****************************
Node machine sends a pong message on receiving a ping
*****************************/
machine Node {
  start state WaitForPing {
    on inePing do (req: tinePing) {
      send req.controller, inePong, (controller = req.controller, dst = req.fd, trial = req.trial);
    }

    on eShutDown do {
      raise halt;
    }
  }
}