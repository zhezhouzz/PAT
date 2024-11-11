/****************************
Node machine sends a pong message on receiving a ping
*****************************/
machine Node {
  start state WaitForPing {
    on ePing do (req: tePing) {
      if ($) {
        RealSend(req.fd, ePong, (controller = req.controller, dst = req.fd, trial = req.trial));
      }
    }

    on eShutDown do {
      raise halt;
    }
  }
}