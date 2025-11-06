machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_bool: set[bool], domain_tCmdStatus: set[tCmdStatus], domain_tGid: set[tGid], domain_tKey: set[tKey], domain_tTxnStatus: set[tTxnStatus], domain_tVal: set[tVal])) {
      var setting: setting;
      var domain_bool: set[bool];
      var domain_tCmdStatus: set[tCmdStatus];
      var domain_tGid: set[tGid];
      var domain_tKey: set[tKey];
      var domain_tTxnStatus: set[tTxnStatus];
      var domain_tVal: set[tVal];
      var input_eStartTxnRsp: (gid: tGid);
      var __y0: tGid;
      var __x2: tKey;
      var __x3: tVal;
      var input_eShardUpdateKeyReq: (gid: tGid, key: tKey, value: tVal);
      var __y4: tGid;
      var __y5: tKey;
      var __y6: tVal;
      var __y7: tGid;
      var __x9: tVal;
      var __y10: tGid;
      var __y11: tKey;
      var __y12: tVal;
      var input_eShardUpdateKeyRsp: (gid: tGid, key: tKey, value: tVal, status: tCmdStatus);
      var __y13: tGid;
      var __y14: tKey;
      var __y15: tVal;
      var __y16: tCmdStatus;
      var input_eUpdateRsp: (gid: tGid, key: tKey, value: tVal, status: tCmdStatus);
      var __y17: tGid;
      var __y18: tKey;
      var __y19: tVal;
      var __y20: tCmdStatus;
      var input_eShardReadKeyReq: (gid: tGid, key: tKey);
      var __y21: tGid;
      var __y22: tKey;
      var input_eShardReadKeyRsp: (gid: tGid, key: tKey, value: tVal, status: tCmdStatus);
      var __y23: tGid;
      var __y24: tKey;
      var __y25: tVal;
      var __y26: tCmdStatus;
      var input_eReadRsp: (gid: tGid, key: tKey, value: tVal, status: tCmdStatus);
      var __y27: tGid;
      var __y28: tKey;
      var __y29: tVal;
      var __y30: tCmdStatus;
      var __y31: tGid;
      var __y32: tKey;
      var __y33: tVal;
      var __y34: tCmdStatus;
      var __y35: tGid;
      var __y36: tKey;
      var __y37: tVal;
      var __y38: tCmdStatus;
      setting = input.setting;
      domain_bool = input.domain_bool;
      domain_tCmdStatus = input.domain_tCmdStatus;
      domain_tGid = input.domain_tGid;
      domain_tKey = input.domain_tKey;
      domain_tTxnStatus = input.domain_tTxnStatus;
      domain_tVal = input.domain_tVal;
      send_eStartTxnReq(this, setting);
      receive { case syn_eStartTxnRsp: (input: tsyn_eStartTxnRsp) {
        announce an_syn_eStartTxnRsp, input;
        input_eStartTxnRsp = cast_syn_eStartTxnRsp(input);
        __y0 = input_eStartTxnRsp.gid;
      }};
      assert true;
      while(true){
        __x2 = choose(domain_tKey);
        __x3 = choose(domain_tVal);
        if (true) {
          break;
        };
      };
      send_eUpdateReq(this, setting, (gid = __y0, key = __x2, value = __x3));
      receive { case syn_eShardUpdateKeyReq: (input: tsyn_eShardUpdateKeyReq) {
        announce an_syn_eShardUpdateKeyReq, input;
        forward_syn_eShardUpdateKeyReq(input);
        input_eShardUpdateKeyReq = cast_syn_eShardUpdateKeyReq(input);
        __y4 = input_eShardUpdateKeyReq.gid;
        __y5 = input_eShardUpdateKeyReq.key;
        __y6 = input_eShardUpdateKeyReq.value;
      }};
      assert true;
      send_eStartTxnReq(this, setting);
      receive { case syn_eStartTxnRsp: (input: tsyn_eStartTxnRsp) {
        announce an_syn_eStartTxnRsp, input;
        input_eStartTxnRsp = cast_syn_eStartTxnRsp(input);
        __y7 = input_eStartTxnRsp.gid;
      }};
      assert true;
      while(true){
        __x9 = choose(domain_tVal);
        if ((!((__x3 == __x9)) && !((__x9 == __x3)))) {
          break;
        };
      };
      send_eUpdateReq(this, setting, (gid = __y7, key = __x2, value = __x9));
      receive { case syn_eShardUpdateKeyReq: (input: tsyn_eShardUpdateKeyReq) {
        announce an_syn_eShardUpdateKeyReq, input;
        forward_syn_eShardUpdateKeyReq(input);
        input_eShardUpdateKeyReq = cast_syn_eShardUpdateKeyReq(input);
        __y10 = input_eShardUpdateKeyReq.gid;
        __y11 = input_eShardUpdateKeyReq.key;
        __y12 = input_eShardUpdateKeyReq.value;
      }};
      assert true;
      receive { case syn_eShardUpdateKeyRsp: (input: tsyn_eShardUpdateKeyRsp) {
        announce an_syn_eShardUpdateKeyRsp, input;
        forward_syn_eShardUpdateKeyRsp(input);
        input_eShardUpdateKeyRsp = cast_syn_eShardUpdateKeyRsp(input);
        __y13 = input_eShardUpdateKeyRsp.gid;
        __y14 = input_eShardUpdateKeyRsp.key;
        __y15 = input_eShardUpdateKeyRsp.value;
        __y16 = input_eShardUpdateKeyRsp.status;
      }};
      assert true;
      receive { case syn_eUpdateRsp: (input: tsyn_eUpdateRsp) {
        announce an_syn_eUpdateRsp, input;
        input_eUpdateRsp = cast_syn_eUpdateRsp(input);
        __y17 = input_eUpdateRsp.gid;
        __y18 = input_eUpdateRsp.key;
        __y19 = input_eUpdateRsp.value;
        __y20 = input_eUpdateRsp.status;
      }};
      assert true;
      send_eReadReq(this, setting, (gid = __y7, key = __x2));
      receive { case syn_eShardReadKeyReq: (input: tsyn_eShardReadKeyReq) {
        announce an_syn_eShardReadKeyReq, input;
        forward_syn_eShardReadKeyReq(input);
        input_eShardReadKeyReq = cast_syn_eShardReadKeyReq(input);
        __y21 = input_eShardReadKeyReq.gid;
        __y22 = input_eShardReadKeyReq.key;
      }};
      assert true;
      receive { case syn_eShardReadKeyRsp: (input: tsyn_eShardReadKeyRsp) {
        announce an_syn_eShardReadKeyRsp, input;
        forward_syn_eShardReadKeyRsp(input);
        input_eShardReadKeyRsp = cast_syn_eShardReadKeyRsp(input);
        __y23 = input_eShardReadKeyRsp.gid;
        __y24 = input_eShardReadKeyRsp.key;
        __y25 = input_eShardReadKeyRsp.value;
        __y26 = input_eShardReadKeyRsp.status;
      }};
      assert true;
      receive { case syn_eReadRsp: (input: tsyn_eReadRsp) {
        announce an_syn_eReadRsp, input;
        input_eReadRsp = cast_syn_eReadRsp(input);
        __y27 = input_eReadRsp.gid;
        __y28 = input_eReadRsp.key;
        __y29 = input_eReadRsp.value;
        __y30 = input_eReadRsp.status;
      }};
      assert true;
      receive { case syn_eShardUpdateKeyRsp: (input: tsyn_eShardUpdateKeyRsp) {
        announce an_syn_eShardUpdateKeyRsp, input;
        forward_syn_eShardUpdateKeyRsp(input);
        input_eShardUpdateKeyRsp = cast_syn_eShardUpdateKeyRsp(input);
        __y31 = input_eShardUpdateKeyRsp.gid;
        __y32 = input_eShardUpdateKeyRsp.key;
        __y33 = input_eShardUpdateKeyRsp.value;
        __y34 = input_eShardUpdateKeyRsp.status;
      }};
      assert true;
      receive { case syn_eUpdateRsp: (input: tsyn_eUpdateRsp) {
        announce an_syn_eUpdateRsp, input;
        input_eUpdateRsp = cast_syn_eUpdateRsp(input);
        __y35 = input_eUpdateRsp.gid;
        __y36 = input_eUpdateRsp.key;
        __y37 = input_eUpdateRsp.value;
        __y38 = input_eUpdateRsp.status;
      }};
      assert true;
    }

  }
}

