event an_syn_eWithDrawResp: tsyn_eWithDrawResp;
event an_syn_eReadQuery: tsyn_eReadQuery;
event an_syn_eReadQueryResp: tsyn_eReadQueryResp;

spec bank_safe observes an_syn_eWithDrawResp {
  start state Init {
    entry{
	    }
    on an_syn_eWithDrawResp do (input: tsyn_eWithDrawResp) {
      assert (input.status), "property violation";
    }
  }
}