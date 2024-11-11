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
            if (input.accountId in balanceMap) {
                RealSend(bank, syn_eReadQueryResp, (controller = input.controller, dst = bank, rId = input.rId, amount = input.amount, accountId = input.accountId, balance = balanceMap[input.accountId]));
            }
        }

        on syn_eUpdateQuery do (input: tsyn_eUpdateQuery) {
            if (input.accountId in balanceMap) {
                balanceMap[input.accountId] = input.balance;
            }
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
            RealSend(db, eInitAccount, (bank = this, accountId = input.accountId, balance = input.balance));
            goto Wait;
        }
        ignore syn_eWithDrawReq;
        
    }
    state Wait {
        entry{}
        on syn_eWithDrawReq do (input: tsyn_eWithDrawReq) {
            RealSend(db, syn_eReadQuery, (controller = input.controller, dst = db, rId = input.rId, amount = input.amount, accountId = input.accountId));
        }
        on syn_eReadQueryResp do (input: tsyn_eReadQueryResp) {
            var new_balance: int;
            new_balance = input.balance - input.amount;
            if (new_balance > 0) {
                RealSend(db, syn_eUpdateQuery, (controller = input.controller, dst = db, accountId = input.accountId, balance = new_balance)); 
                RealSend(input.controller, syn_eWithDrawResp, (controller = input.controller, dst = input.controller, rId = input.rId, accountId = input.accountId, balance = new_balance, status = true));
            } else {
                RealSend(input.controller, syn_eWithDrawResp, (controller = input.controller, dst = input.controller, rId = input.rId, accountId = input.accountId, balance = new_balance, status = false));
            }
        }
    }
}