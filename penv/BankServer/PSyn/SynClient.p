machine SynClient {
  start state Syn {
    entry (input: (setting: machine, domain_int: set[int], domain_bool: set[bool], domain_aid: set[int], domain_rid: set[int])) {
      var setting: machine;
      var domain_int: set[int];
      var domain_bool: set[bool];
      var domain_aid: set[int];
      var domain_rid: set[int];
      var ac: aid;
      var ba_5: int;
      var id_0: rid;
      var am_2: int;
      var input_eReadQuery: (rId: rid, amount: int, accountId: aid);
      var tmp_6: rid;
      var tmp_7: int;
      var tmp_8: aid;
      var input_eReadQueryResp: (rId: rid, amount: int, accountId: aid, balance: int);
      var tmp_2: rid;
      var tmp_3: int;
      var tmp_4: aid;
      var tmp_5: int;
      var input_eWithDrawResp: (rId: rid, accountId: aid, balance: int, status: bool);
      var tmp_0: rid;
      var tmp_1: aid;
      var ba_0: int;
      var st_0: bool;
      setting = input.setting;
      domain_int = input.domain_int;
      domain_bool = input.domain_bool;
      domain_aid = input.domain_aid;
      domain_rid = input.domain_rid;
      while(true){
        ac = choose(domain_aid);
        ba_5 = choose(domain_int);
        if ((!((ba_5 == 0)) && !((0 > ba_5)))) {
          break;
        };
      };
      send_eInitAccount(this, setting, (accountId = ac, balance = ba_5));
      while(true){
        id_0 = choose(domain_rid);
        am_2 = choose(domain_int);
        if ((((((ba_5 == am_2) && !((am_2 == 0))) && !((am_2 > ba_5))) && !((0 > am_2))) || (((!((ba_5 == am_2)) && !((am_2 == 0))) && (am_2 > ba_5)) && !((0 > am_2))))) {
          break;
        };
      };
      send_eWithDrawReq(this, setting, (rId = id_0, accountId = ac, amount = am_2));
      receive { case syn_eReadQuery: (input: tsyn_eReadQuery) {
        forward_syn_eReadQuery(input);
        input_eReadQuery = cast_syn_eReadQuery(input);
        tmp_6 = input_eReadQuery.rId;
        tmp_7 = input_eReadQuery.amount;
        tmp_8 = input_eReadQuery.accountId;
      }};
      assert (((tmp_6 == id_0) && (tmp_7 == am_2)) && (tmp_8 == ac));
      receive { case syn_eReadQueryResp: (input: tsyn_eReadQueryResp) {
        forward_syn_eReadQueryResp(input);
        input_eReadQueryResp = cast_syn_eReadQueryResp(input);
        tmp_2 = input_eReadQueryResp.rId;
        tmp_3 = input_eReadQueryResp.amount;
        tmp_4 = input_eReadQueryResp.accountId;
        tmp_5 = input_eReadQueryResp.balance;
      }};
      assert ((((tmp_2 == id_0) && (tmp_3 == am_2)) && (tmp_4 == ac)) && (tmp_5 == ba_5));
      receive { case syn_eWithDrawResp: (input: tsyn_eWithDrawResp) {
        forward_syn_eWithDrawResp(input);
        input_eWithDrawResp = cast_syn_eWithDrawResp(input);
        tmp_0 = input_eWithDrawResp.rId;
        tmp_1 = input_eWithDrawResp.accountId;
        ba_0 = input_eWithDrawResp.balance;
        st_0 = input_eWithDrawResp.status;
      }};
      assert (((tmp_0 == id_0) && (tmp_1 == ac)) && (((((((((((((!(st_0) && !((ba_5 == ba_0))) && !((ba_0 == am_2))) && !((ba_0 == 0))) && !((ba_0 > ba_5))) && !((ba_0 > am_2))) && !((0 > ba_0))) || ((((((!(st_0) && (ba_5 == ba_0)) && !((ba_0 == am_2))) && !((ba_0 == 0))) && !((ba_0 > ba_5))) && !((ba_0 > am_2))) && !((0 > ba_0)))) || ((((((!(st_0) && (ba_5 == ba_0)) && (ba_0 == am_2)) && !((ba_0 == 0))) && !((ba_0 > ba_5))) && !((ba_0 > am_2))) && !((0 > ba_0)))) || ((((((!(st_0) && !((ba_5 == ba_0))) && !((ba_0 == am_2))) && (ba_0 == 0)) && !((ba_0 > ba_5))) && !((ba_0 > am_2))) && !((0 > ba_0)))) || ((((((!(st_0) && !((ba_5 == ba_0))) && !((ba_0 == am_2))) && !((ba_0 == 0))) && (ba_0 > ba_5)) && !((ba_0 > am_2))) && !((0 > ba_0)))) || ((((((!(st_0) && !((ba_5 == ba_0))) && (ba_0 == am_2)) && !((ba_0 == 0))) && (ba_0 > ba_5)) && !((ba_0 > am_2))) && !((0 > ba_0)))) || ((((((!(st_0) && !((ba_5 == ba_0))) && !((ba_0 == am_2))) && !((ba_0 == 0))) && (ba_0 > ba_5)) && (ba_0 > am_2)) && !((0 > ba_0)))) || ((((((!(st_0) && !((ba_5 == ba_0))) && !((ba_0 == am_2))) && !((ba_0 == 0))) && !((ba_0 > ba_5))) && !((ba_0 > am_2))) && (0 > ba_0))));
    }

  }
}

