machine Database {
    var balanceMap: map[int, int];
    var bank: machine;
    start state Init {
        entry {
            receive { 
                case eInitAccount: (input: teInitAccount) {
                    balanceMap[input.accountId] = input.balance;
                    bank = input.bank;
                }
            }
        }
        on syn_eReadQuery do (input: tsyn_eReadQuery) {
            send input.controller, syn_eReadQueryResp, (controller = input.controller, dst = bank, rId = input.rId, amount = input.amount, accountId = input.accountId, balance = balanceMap[input.accountId]);
        }

        on syn_eUpdateQuery do (input: tsyn_eUpdateQuery) {
            balanceMap[input.accountId] = input.balance;
        }
    } 
}

machine Bank {
    var is_init: bool;
    var db: machine;
    start state Init {
        entry (database: machine){
            db = database;
        }

        on syn_eInitAccount do (input: tsyn_eInitAccount) {
            send db, eInitAccount, (bank = this, accountId = input.accountId, balance = input.balance);
            goto Wait;
        }
        
    }
    state Wait {
        entry{}
        on syn_eWithDrawReq do (input: tsyn_eWithDrawReq) {
            send input.controller, syn_eReadQuery, (controller = input.controller, dst = db, rId = input.rId, amount = input.amount, accountId = input.accountId);
        }
        on syn_eReadQueryResp do (input: tsyn_eReadQueryResp) {
            var new_balance: int;
            new_balance = input.balance - input.amount;
            if (new_balance > 0) {
                send input.controller, syn_eUpdateQuery, (controller = input.controller, dst = db, accountId = input.accountId, balance = new_balance); 
                send input.controller, syn_eWithDrawResp, (controller = input.controller, dst = input.controller, rId = input.rId, accountId = input.accountId, balance = new_balance, status = true);
            } else {
                send input.controller, syn_eWithDrawResp, (controller = input.controller, dst = input.controller, rId = input.rId, accountId = input.accountId, balance = new_balance, status = false);
            }
        }
    }
}