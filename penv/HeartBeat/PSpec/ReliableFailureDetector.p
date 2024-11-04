spec ReliableFailureDetector observes syn_eNotifyNodesDown, syn_eShutDown {
  var is_shut_down: bool;
  start state Init {
    entry{
      is_shut_down = false;
    }
    on syn_eNotifyNodesDown do {
      if (!is_shut_down) {
        assert false, "property violation";
      }
    }
    on syn_eShutDown do {
      is_shut_down = true;
    }
  }
}