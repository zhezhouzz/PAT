event an_syn_eNotifyNodesDown: tsyn_eNotifyNodesDown;
event an_syn_eShutDown: tsyn_eShutDown;
event an_syn_ePong: tsyn_ePong;
event an_syn_ePongLost: tsyn_ePongLost;
event an_syn_ePing: tsyn_ePing;
event an_syn_eNetworkError: tsyn_eNetworkError;
event an_syn_eStart: tsyn_eStart;

spec ReliableFailureDetector observes an_syn_eNotifyNodesDown, an_syn_eShutDown {
  var is_shut_down: bool;
  start state Init {
    entry{
      is_shut_down = false;
    }
    on an_syn_eNotifyNodesDown do {
      if (!is_shut_down) {
        assert false, "property violation";
      }
    }
    on an_syn_eShutDown do {
      is_shut_down = true;
    }
  }
}