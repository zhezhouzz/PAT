fun send_eWithDrawReq (src: machine, dest: machine, input: (rId: rid, accountId: aid, amount: int)) {
  send dest, syn_eWithDrawReq, (controller = src, dst = dest, rId = input.rId, accountId = input.accountId, amount = input.amount);
}

fun send_eInitAccount (src: machine, dest: machine, input: (accountId: aid, balance: int)) {
  send dest, syn_eInitAccount, (controller = src, dst = dest, accountId = input.accountId, balance = input.balance);
}



fun cast_syn_eWithDrawResp (input: tsyn_eWithDrawResp): (rId: rid, accountId: aid, balance: int, status: bool) {
  return (rId = input.rId, accountId = input.accountId, balance = input.balance, status = input.status);
}

fun cast_syn_eUpdateQuery (input: tsyn_eUpdateQuery): (accountId: aid, balance: int) {
  return (accountId = input.accountId, balance = input.balance);
}

fun cast_syn_eReadQueryResp (input: tsyn_eReadQueryResp): (rId: rid, amount: int, accountId: aid, balance: int) {
  return (rId = input.rId, amount = input.amount, accountId = input.accountId, balance = input.balance);
}

fun cast_syn_eReadQuery (input: tsyn_eReadQuery): (rId: rid, amount: int, accountId: aid) {
  return (rId = input.rId, amount = input.amount, accountId = input.accountId);
}



fun forward_syn_eWithDrawResp (input: tsyn_eWithDrawResp) {
  send input.dst, syn_eWithDrawResp, input;
}

fun forward_syn_eWithDrawReq (input: tsyn_eWithDrawReq) {
  send input.dst, syn_eWithDrawReq, input;
}

fun forward_syn_eUpdateQuery (input: tsyn_eUpdateQuery) {
  send input.dst, syn_eUpdateQuery, input;
}

fun forward_syn_eReadQueryResp (input: tsyn_eReadQueryResp) {
  send input.dst, syn_eReadQueryResp, input;
}

fun forward_syn_eReadQuery (input: tsyn_eReadQuery) {
  send input.dst, syn_eReadQuery, input;
}

fun forward_syn_eInitAccount (input: tsyn_eInitAccount) {
  send input.dst, syn_eInitAccount, input;
}