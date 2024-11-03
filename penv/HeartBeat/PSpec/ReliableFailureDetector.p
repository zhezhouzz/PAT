spec ReliableFailureDetector observes eNotifyNodesDown, eShutDown {
  var is_shut_down: bool;
  start state Init {
    entry{
      is_shut_down = false;
    }
    on eNotifyNodesDown do {
      if (!is_shut_down) {
        assert false, "property violation";
      }
    }
    on eShutDown do {
      is_shut_down = true;
    }
  }
}