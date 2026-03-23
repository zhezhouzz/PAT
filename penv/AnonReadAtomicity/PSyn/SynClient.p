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
      var __x1: tGid;
      var __x2: tKey;
      var __x3: tVal;
      var input_eShardUpdateKeyReq: (gid: tGid, key: tKey, value: tVal);
      var __y4: tGid;
      var __y5: tKey;
      var __y6: tVal;
      var input_eShardUpdateKeyRsp: (gid: tGid, key: tKey, value: tVal, status: tCmdStatus);
      var __y7: tGid;
      var __y8: tKey;
      var __y9: tVal;
      var __y10: tCmdStatus;
      var input_eUpdateRsp: (gid: tGid, key: tKey, value: tVal, status: tCmdStatus);
      var __y11: tGid;
      var __y12: tKey;
      var __y13: tVal;
      var __y14: tCmdStatus;
      var __x15: tGid;
      var __x16: tKey;
      var input_eShardReadKeyReq: (gid: tGid, key: tKey);
      var __y17: tGid;
      var __y18: tKey;
      var input_eShardReadKeyRsp: (gid: tGid, key: tKey, value: tVal, status: tCmdStatus);
      var __y19: tGid;
      var __y20: tKey;
      var __y21: tVal;
      var __y22: tCmdStatus;
      var input_eReadRsp: (gid: tGid, key: tKey, value: tVal, status: tCmdStatus);
      var __y23: tGid;
      var __y24: tKey;
      var __y25: tVal;
      var __y26: tCmdStatus;
      var __y27: tGid;
      var __y28: tKey;
      var __y29: tVal;
      var __y30: tCmdStatus;
      var __y31: tGid;
      var __y32: tKey;
      var __y33: tVal;
      var __y34: tCmdStatus;
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
        __x1 = choose(domain_tGid);
        __x2 = choose(domain_tKey);
        __x3 = choose(domain_tVal);
        if (true) {
          break;
        };
      };
      send_eUpdateReq(this, setting, (gid = __x1, key = __x2, value = __x3));
      receive { case syn_eShardUpdateKeyReq: (input: tsyn_eShardUpdateKeyReq) {
        announce an_syn_eShardUpdateKeyReq, input;
        forward_syn_eShardUpdateKeyReq(input);
        input_eShardUpdateKeyReq = cast_syn_eShardUpdateKeyReq(input);
        __y4 = input_eShardUpdateKeyReq.gid;
        __y5 = input_eShardUpdateKeyReq.key;
        __y6 = input_eShardUpdateKeyReq.value;
      }};
      assert true;
      receive { case syn_eShardUpdateKeyRsp: (input: tsyn_eShardUpdateKeyRsp) {
        announce an_syn_eShardUpdateKeyRsp, input;
        forward_syn_eShardUpdateKeyRsp(input);
        input_eShardUpdateKeyRsp = cast_syn_eShardUpdateKeyRsp(input);
        __y7 = input_eShardUpdateKeyRsp.gid;
        __y8 = input_eShardUpdateKeyRsp.key;
        __y9 = input_eShardUpdateKeyRsp.value;
        __y10 = input_eShardUpdateKeyRsp.status;
      }};
      assert true;
      receive { case syn_eUpdateRsp: (input: tsyn_eUpdateRsp) {
        announce an_syn_eUpdateRsp, input;
        input_eUpdateRsp = cast_syn_eUpdateRsp(input);
        __y11 = input_eUpdateRsp.gid;
        __y12 = input_eUpdateRsp.key;
        __y13 = input_eUpdateRsp.value;
        __y14 = input_eUpdateRsp.status;
      }};
      assert true;
      while(true){
        __x15 = choose(domain_tGid);
        __x16 = choose(domain_tKey);
        if (true) {
          break;
        };
      };
      send_eReadReq(this, setting, (gid = __x15, key = __x16));
      receive { case syn_eShardReadKeyReq: (input: tsyn_eShardReadKeyReq) {
        announce an_syn_eShardReadKeyReq, input;
        forward_syn_eShardReadKeyReq(input);
        input_eShardReadKeyReq = cast_syn_eShardReadKeyReq(input);
        __y17 = input_eShardReadKeyReq.gid;
        __y18 = input_eShardReadKeyReq.key;
      }};
      assert true;
      receive { case syn_eShardReadKeyRsp: (input: tsyn_eShardReadKeyRsp) {
        announce an_syn_eShardReadKeyRsp, input;
        forward_syn_eShardReadKeyRsp(input);
        input_eShardReadKeyRsp = cast_syn_eShardReadKeyRsp(input);
        __y19 = input_eShardReadKeyRsp.gid;
        __y20 = input_eShardReadKeyRsp.key;
        __y21 = input_eShardReadKeyRsp.value;
        __y22 = input_eShardReadKeyRsp.status;
      }};
      assert true;
      receive { case syn_eReadRsp: (input: tsyn_eReadRsp) {
        announce an_syn_eReadRsp, input;
        input_eReadRsp = cast_syn_eReadRsp(input);
        __y23 = input_eReadRsp.gid;
        __y24 = input_eReadRsp.key;
        __y25 = input_eReadRsp.value;
        __y26 = input_eReadRsp.status;
      }};
      assert true;
      receive { case syn_eShardUpdateKeyRsp: (input: tsyn_eShardUpdateKeyRsp) {
        announce an_syn_eShardUpdateKeyRsp, input;
        forward_syn_eShardUpdateKeyRsp(input);
        input_eShardUpdateKeyRsp = cast_syn_eShardUpdateKeyRsp(input);
        __y27 = input_eShardUpdateKeyRsp.gid;
        __y28 = input_eShardUpdateKeyRsp.key;
        __y29 = input_eShardUpdateKeyRsp.value;
        __y30 = input_eShardUpdateKeyRsp.status;
      }};
      assert true;
      receive { case syn_eUpdateRsp: (input: tsyn_eUpdateRsp) {
        announce an_syn_eUpdateRsp, input;
        input_eUpdateRsp = cast_syn_eUpdateRsp(input);
        __y31 = input_eUpdateRsp.gid;
        __y32 = input_eUpdateRsp.key;
        __y33 = input_eUpdateRsp.value;
        __y34 = input_eUpdateRsp.status;
      }};
      assert true;
    }

  }
}

