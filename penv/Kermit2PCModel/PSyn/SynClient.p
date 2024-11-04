machine SynClient {
  start state Syn {
    entry (input: (setting: setting, domain_bool: set[bool], domain_tGid: set[int], domain_tKey: set[int], domain_tVal: set[int], domain_tCmdStatus: set[tCmdStatus], domain_tTxnStatus: set[tTxnStatus])) {
      var setting: setting;
      var domain_bool: set[bool];
      var domain_tGid: set[int];
      var domain_tKey: set[int];
      var domain_tVal: set[int];
      var domain_tCmdStatus: set[tCmdStatus];
      var domain_tTxnStatus: set[tTxnStatus];
      var input_eStartTxnRsp: (gid: tGid);
      var id: tGid;
      var k: tKey;
      var v2: tVal;
      var input_eShardUpdateKeyReq: (gid: tGid, key: tKey, value: tVal);
      var tmp_28: tGid;
      var tmp_29: tKey;
      var tmp_30: tVal;
      var input_eShardUpdateKeyRsp: (gid: tGid, key: tKey, value: tVal, status: tCmdStatus);
      var tmp_25: tGid;
      var tmp_26: tKey;
      var tmp_27: tVal;
      var st_0: tCmdStatus;
      var input_eUpdateRsp: (gid: tGid, key: tKey, value: tVal, status: tCmdStatus);
      var tmp_21: tGid;
      var tmp_22: tKey;
      var tmp_23: tVal;
      var tmp_24: tCmdStatus;
      var v1: tVal;
      var tmp_18: tGid;
      var tmp_19: tKey;
      var tmp_20: tVal;
      var tmp_14: tGid;
      var tmp_15: tKey;
      var tmp_16: tVal;
      var tmp_17: tCmdStatus;
      var tmp_10: tGid;
      var tmp_11: tKey;
      var tmp_12: tVal;
      var tmp_13: tCmdStatus;
      var input_eShardReadKeyReq: (gid: tGid, key: tKey);
      var tmp_8: tGid;
      var tmp_9: tKey;
      var input_eShardReadKeyRsp: (gid: tGid, key: tKey, value: tVal, status: tCmdStatus);
      var tmp_4: tGid;
      var tmp_5: tKey;
      var tmp_6: tVal;
      var tmp_7: tCmdStatus;
      var input_eReadRsp: (gid: tGid, key: tKey, value: tVal, status: tCmdStatus);
      var tmp_0: tGid;
      var tmp_1: tKey;
      var tmp_2: tVal;
      var tmp_3: tCmdStatus;
      setting = input.setting;
      domain_bool = input.domain_bool;
      domain_tGid = input.domain_tGid;
      domain_tKey = input.domain_tKey;
      domain_tVal = input.domain_tVal;
      domain_tCmdStatus = input.domain_tCmdStatus;
      domain_tTxnStatus = input.domain_tTxnStatus;
      send_eStartTxnReq(this, setting);
      receive { case syn_eStartTxnRsp: (input: tsyn_eStartTxnRsp) {
        input_eStartTxnRsp = cast_syn_eStartTxnRsp(input);
        id = input_eStartTxnRsp.gid;
      }};
      assert true;
      while(true){
        k = choose(domain_tKey);
        v2 = choose(domain_tVal);
        if (true) {
          break;
        };
      };
      send_eUpdateReq(this, setting, (gid = id, key = k, value = v2));
      receive { case syn_eShardUpdateKeyReq: (input: tsyn_eShardUpdateKeyReq) {
        forward_syn_eShardUpdateKeyReq(input);
        input_eShardUpdateKeyReq = cast_syn_eShardUpdateKeyReq(input);
        tmp_28 = input_eShardUpdateKeyReq.gid;
        tmp_29 = input_eShardUpdateKeyReq.key;
        tmp_30 = input_eShardUpdateKeyReq.value;
      }};
      assert (((tmp_28 == id) && (tmp_29 == k)) && (tmp_30 == v2));
      receive { case syn_eShardUpdateKeyRsp: (input: tsyn_eShardUpdateKeyRsp) {
        forward_syn_eShardUpdateKeyRsp(input);
        input_eShardUpdateKeyRsp = cast_syn_eShardUpdateKeyRsp(input);
        tmp_25 = input_eShardUpdateKeyRsp.gid;
        tmp_26 = input_eShardUpdateKeyRsp.key;
        tmp_27 = input_eShardUpdateKeyRsp.value;
        st_0 = input_eShardUpdateKeyRsp.status;
      }};
      assert ((((tmp_25 == id) && (tmp_26 == k)) && (tmp_27 == v2)) && (st_0 == OK));
      receive { case syn_eUpdateRsp: (input: tsyn_eUpdateRsp) {
        input_eUpdateRsp = cast_syn_eUpdateRsp(input);
        tmp_21 = input_eUpdateRsp.gid;
        tmp_22 = input_eUpdateRsp.key;
        tmp_23 = input_eUpdateRsp.value;
        tmp_24 = input_eUpdateRsp.status;
      }};
      assert ((((tmp_21 == id) && (tmp_22 == k)) && (tmp_23 == v2)) && (tmp_24 == st_0));
      while(true){
        v1 = choose(domain_tVal);
        if (!((v2 == v1))) {
          break;
        };
      };
      send_eUpdateReq(this, setting, (gid = id, key = k, value = v1));
      receive { case syn_eShardUpdateKeyReq: (input: tsyn_eShardUpdateKeyReq) {
        forward_syn_eShardUpdateKeyReq(input);
        input_eShardUpdateKeyReq = cast_syn_eShardUpdateKeyReq(input);
        tmp_18 = input_eShardUpdateKeyReq.gid;
        tmp_19 = input_eShardUpdateKeyReq.key;
        tmp_20 = input_eShardUpdateKeyReq.value;
      }};
      assert (((tmp_18 == id) && (tmp_19 == k)) && (tmp_20 == v1));
      receive { case syn_eShardUpdateKeyRsp: (input: tsyn_eShardUpdateKeyRsp) {
        forward_syn_eShardUpdateKeyRsp(input);
        input_eShardUpdateKeyRsp = cast_syn_eShardUpdateKeyRsp(input);
        tmp_14 = input_eShardUpdateKeyRsp.gid;
        tmp_15 = input_eShardUpdateKeyRsp.key;
        tmp_16 = input_eShardUpdateKeyRsp.value;
        tmp_17 = input_eShardUpdateKeyRsp.status;
      }};
      assert ((((tmp_14 == id) && (tmp_15 == k)) && (tmp_16 == v1)) && (tmp_17 == st_0));
      receive { case syn_eUpdateRsp: (input: tsyn_eUpdateRsp) {
        input_eUpdateRsp = cast_syn_eUpdateRsp(input);
        tmp_10 = input_eUpdateRsp.gid;
        tmp_11 = input_eUpdateRsp.key;
        tmp_12 = input_eUpdateRsp.value;
        tmp_13 = input_eUpdateRsp.status;
      }};
      assert ((((tmp_10 == id) && (tmp_11 == k)) && (tmp_12 == v1)) && (tmp_13 == st_0));
      send_eReadReq(this, setting, (gid = id, key = k));
      receive { case syn_eShardReadKeyReq: (input: tsyn_eShardReadKeyReq) {
        forward_syn_eShardReadKeyReq(input);
        input_eShardReadKeyReq = cast_syn_eShardReadKeyReq(input);
        tmp_8 = input_eShardReadKeyReq.gid;
        tmp_9 = input_eShardReadKeyReq.key;
      }};
      assert ((tmp_8 == id) && (tmp_9 == k));
      receive { case syn_eShardReadKeyRsp: (input: tsyn_eShardReadKeyRsp) {
        forward_syn_eShardReadKeyRsp(input);
        input_eShardReadKeyRsp = cast_syn_eShardReadKeyRsp(input);
        tmp_4 = input_eShardReadKeyRsp.gid;
        tmp_5 = input_eShardReadKeyRsp.key;
        tmp_6 = input_eShardReadKeyRsp.value;
        tmp_7 = input_eShardReadKeyRsp.status;
      }};
      assert ((((tmp_4 == id) && (tmp_5 == k)) && (tmp_6 == v2)) && (tmp_7 == st_0));
      receive { case syn_eReadRsp: (input: tsyn_eReadRsp) {
        input_eReadRsp = cast_syn_eReadRsp(input);
        tmp_0 = input_eReadRsp.gid;
        tmp_1 = input_eReadRsp.key;
        tmp_2 = input_eReadRsp.value;
        tmp_3 = input_eReadRsp.status;
      }};
      assert ((((tmp_0 == id) && (tmp_1 == k)) && (tmp_2 == v2)) && (tmp_3 == st_0));
    }

  }
}

