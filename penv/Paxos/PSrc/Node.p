machine Learner {
    var store: int;
    start state Init {
        entry {
        }
        on syn_eLearn do (input: tsyn_eLearn) {
            store = input.va;
        }
    }
}

machine Proposer {
    var acceptorMap: map[tAcceptorNode, machine];
    var leaner: machine;
    var lostPrReq: set [(a: tAcceptorNode, p: tProposerNode)];
    var lostAcReq: set [(a: tAcceptorNode, p: tProposerNode)];
    start state Init {
        entry (ln: machine) {
            leaner = ln;
            receive { 
                case eInit: (input: setting){
                    acceptorMap = input.acceptorMap;
                }
            }
            goto Loop;
        }
    }
    state Loop {
        entry {
            // var i: int;
            // if (sizeof(cache) >= 2) {
            //     i = sizeof(cache) - 1;
            //     while(i >= 0) {
            //         do_send_write_to_tail(cache[i]);
            //         i = i - 1;
            //     }
            //     cache = default(seq[tsyn_writeToTail]);
            // }
        }
        on syn_eStart do (input: tsyn_eStart) {
            unreadlizablePrepareReq((controller = input.controller, dst = acceptorMap[Acceptor1], proposer = input.proposer, acceptor = Acceptor1, va = input.va));
            unreadlizablePrepareReq((controller = input.controller, dst = acceptorMap[Acceptor2], proposer = input.proposer, acceptor = Acceptor2, va = input.va));
            goto Loop;
        }
        on syn_eLostPrepareReq do (input: tsyn_eLostPrepareReq) {
            lostPrReq += ((a = input.acceptor, p = input.proposer));
            goto Loop;
        }
        on syn_eLostAcceptReq do (input: tsyn_eLostAcceptReq) {
            lostAcReq += ((a = input.acceptor, p = input.proposer));
            goto Loop;
        } 
        on syn_ePrepareRsp do (input: tsyn_ePrepareRsp) {
            unreadlizableAcceptReq((controller = input.controller, dst = acceptorMap[input.acceptor], proposer = input.promised, acceptor = input.acceptor, va = input.va));
            goto Loop;
        }
        on syn_eAcceptRsp do (input: tsyn_eAcceptRsp) {
            send input.controller, syn_eLearn, (controller = input.controller, dst = leaner, va = input.va);
            goto Loop;
        }
    }
    fun unreadlizablePrepareReq (input: tsyn_ePrepareReq) {
        var pair: (a: tAcceptorNode, p: tProposerNode);
        pair.a = input.acceptor;
        pair.p = input.proposer;
        if (!(pair in lostPrReq)) {
            send input.controller, syn_ePrepareReq, input;
        }
    }
    fun unreadlizableAcceptReq (input: tsyn_eAcceptReq) {
        var pair: (a: tAcceptorNode, p: tProposerNode);
        pair.a = input.acceptor;
        pair.p = input.proposer;
        if (!(pair in lostAcReq)) {
            send input.controller, syn_eAcceptReq, input;
        }
    }
}

machine Acceptor {
    var proposerMap: map[tProposerNode, machine];
    var lostPrRsp: set [(a: tAcceptorNode, p: tProposerNode)];
    var lostAcRsp: set [(a: tAcceptorNode, p: tProposerNode)];
    var cache: set [tsyn_ePrepareRsp];
    start state Init {
        entry {
            receive { 
                case eInit: (input: setting){
                    proposerMap = input.proposerMap;
                }
            }
            goto Loop;
        }
    }
    state Loop {
        entry {
            var i: int;
            // if (sizeof(cache) >= 2) {
            //     i = sizeof(cache) - 1;
            //     while(i >= 0) {
            //         do_send_write_to_tail(cache[i]);
            //         i = i - 1;
            //     }
            //     cache = default(seq[tsyn_writeToTail]);
            // }
        }
        on syn_eLostPrepareRsp do (input: tsyn_eLostPrepareRsp) {
            lostPrRsp += ((a = input.acceptor, p = input.promised));
            goto Loop;
        }
        on syn_eLostAcceptRsp do (input: tsyn_eLostAcceptRsp) {
            lostAcRsp += ((a = input.acceptor, p = input.proposer));
            goto Loop;
        } 
        on syn_ePrepareReq do (input: tsyn_ePrepareReq) {
            unreadlizablePrepareRsp((controller = input.controller, dst = proposerMap[input.proposer], acceptor = input.acceptor, promised = input.proposer, va = input.va, n_accepted = input.proposer));
            goto Loop;
        }
        on syn_eAcceptReq do (input: tsyn_eAcceptReq) {
            unreadlizableAcceptRsp((controller = input.controller, dst = proposerMap[input.proposer], proposer = input.proposer, acceptor = input.acceptor, accepted = input.proposer, va = input.va));
            goto Loop;
        }
    }
    fun unreadlizablePrepareRsp (input: tsyn_ePrepareRsp) {
        var pair: (a: tAcceptorNode, p: tProposerNode);
        pair.a = input.acceptor;
        pair.p = input.promised;
        if (!(pair in lostPrRsp)) {
            send input.controller, syn_ePrepareRsp, input;
        }
    }
    fun unreadlizableAcceptRsp (input: tsyn_eAcceptRsp) {
        var pair: (a: tAcceptorNode, p: tProposerNode);
        pair.a = input.acceptor;
        pair.p = input.proposer;
        if (!(pair in lostAcRsp)) {
            send input.controller, syn_eAcceptRsp, input;
        }
    }
}
