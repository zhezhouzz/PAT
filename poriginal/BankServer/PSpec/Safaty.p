spec bank_safe observes syn_eWithDrawResp {
  start state Init {
    entry{
    }
    on syn_eWithDrawResp do (input: tsyn_eWithDrawResp) {
      assert (input.status), "property violation";
    }
  }
}