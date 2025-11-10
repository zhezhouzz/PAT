spec allow_all_session_from_internal_node observes syn_eInternalReq, syn_eExternalRsp {
  var allowedNodes: set[tNode];
  start state Init {
    entry{}
    on syn_eInternalReq do (input: tsyn_eInternalReq) {
      allowedNodes += (input.node);
    }
    on syn_eExternalRsp do (input: tsyn_eExternalRsp) {
      if (input.node in allowedNodes) {
          assert input.stat, "property violation"; 
      }
    }
  }
}