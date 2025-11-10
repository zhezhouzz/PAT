machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_aid: set[aid], domain_bool: set[bool], domain_int: set[int], domain_rid: set[rid])) {
      var setting: setting;
      var domain_aid: set[aid];
      var domain_bool: set[bool];
      var domain_int: set[int];
      var domain_rid: set[rid];
      var __x0: aid;
      var __x1: int;
      var __x2: rid;
      var __x3: int;
      var input_eReadQuery: (rId: rid, amount: int, accountId: aid);
      var __y4: rid;
      var __y5: int;
      var __y6: aid;
      var input_eReadQueryResp: (rId: rid, amount: int, accountId: aid, balance: int);
      var __y7: rid;
      var __y8: int;
      var __y9: aid;
      var __y10: int;
      var input_eWithDrawResp: (rId: rid, accountId: aid, balance: int, status: bool);
      var __y11: rid;
      var __y12: aid;
      var __y13: int;
      var __y14: bool;
      setting = input.setting;
      domain_aid = input.domain_aid;
      domain_bool = input.domain_bool;
      domain_int = input.domain_int;
      domain_rid = input.domain_rid;
      while(true){
        __x0 = choose(domain_aid);
        __x1 = choose(domain_int);
        if ((__x1 > 0)) {
          break;
        };
      };
      send_eInitAccount(this, setting, (accountId = __x0, balance = __x1));
      while(true){
        __x2 = choose(domain_rid);
        __x3 = choose(domain_int);
        if ((!((__x1 > __x3)) && (__x3 > 0))) {
          break;
        };
      };
      send_eWithDrawReq(this, setting, (rId = __x2, accountId = __x0, amount = __x3));
      receive { case syn_eReadQuery: (input: tsyn_eReadQuery) {
        announce an_syn_eReadQuery, input;
        forward_syn_eReadQuery(input);
        input_eReadQuery = cast_syn_eReadQuery(input);
        __y4 = input_eReadQuery.rId;
        __y5 = input_eReadQuery.amount;
        __y6 = input_eReadQuery.accountId;
      }};
      assert true;
      receive { case syn_eReadQueryResp: (input: tsyn_eReadQueryResp) {
        announce an_syn_eReadQueryResp, input;
        forward_syn_eReadQueryResp(input);
        input_eReadQueryResp = cast_syn_eReadQueryResp(input);
        __y7 = input_eReadQueryResp.rId;
        __y8 = input_eReadQueryResp.amount;
        __y9 = input_eReadQueryResp.accountId;
        __y10 = input_eReadQueryResp.balance;
      }};
      assert true;
      receive { case syn_eWithDrawResp: (input: tsyn_eWithDrawResp) {
        announce an_syn_eWithDrawResp, input;
        forward_syn_eWithDrawResp(input);
        input_eWithDrawResp = cast_syn_eWithDrawResp(input);
        __y11 = input_eWithDrawResp.rId;
        __y12 = input_eWithDrawResp.accountId;
        __y13 = input_eWithDrawResp.balance;
        __y14 = input_eWithDrawResp.status;
      }};
      assert true;
    }

  }
}

