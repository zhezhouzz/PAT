event an_syn_eInternalReq: tsyn_eInternalReq;
event an_syn_eForwardReq: tsyn_eForwardReq;
event an_syn_eExternalReq: tsyn_eExternalReq;
event an_syn_eExternalRsp: tsyn_eExternalRsp;

spec allow_all_session_from_internal_node observes an_syn_eInternalReq, an_syn_eExternalRsp {
  var allowedNodes: set[tNode];
  start state Init {
    entry{}
    on an_syn_eInternalReq do (input: tsyn_eInternalReq) {
      allowedNodes += (input.node);
    }
    on an_syn_eExternalRsp do (input: tsyn_eExternalRsp) {
      if (input.node in allowedNodes) {
          assert input.stat, "property violation"; 
      }
    }
  }
}