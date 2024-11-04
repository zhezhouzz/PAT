machine InitNode {
    var nodes: machine;
    var store: int;
    // var timer: Timer;
    start state Init {
        entry {
            // timer = CreateTimer(this);
            receive {                 
                case eInit: (input: setting){                
                    nodes = input.nodes;
                }
            }
        }
        on syn_eStart do (input: tsyn_eStart) {
        }
        on syn_eClientPut do (input: tsyn_eClientPut) {
            send input.controller, syn_eAppendEntry, (controller = input.controller, dst = nodes, node = Node2, va = input.va);
            send input.controller, syn_eAppendEntry, (controller = input.controller, dst = nodes, node = Node1, va = input.va);
        }
        on syn_eShutDown do (input: tsyn_eShutDown) {
            send input.controller, syn_eTimeout, (controller = input.controller, dst = nodes, dest = Node2);
            send input.controller, syn_eTimeout, (controller = input.controller, dst = nodes, dest = Node1);
        }
    }
}

machine Node {
    var nodes: machine;
    var log: map[tNode, seq[int]];
    var cache: set[tsyn_eVoteReq]; 
    var timer: Timer;
    var leaders: set[tNode];
    start state Init {
        entry {
            timer = CreateTimer(this);
            log[Node1] = default(seq[int]);
            log[Node2] = default(seq[int]);
            receive {                 
                case eInit: (input: setting){                
                    nodes = input.nodes;
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
            //         do_send_vote_req(cache[i]);
            //         i = i - 1;
            //     }
            //     cache = default(set[tsyn_eVoteReq]);
            // }
        }
        on syn_eAppendEntry do (input: tsyn_eAppendEntry) {
            log[input.node] += (sizeof(log[input.node]), input.va);
        }
        on syn_eTimeout do (input: tsyn_eTimeout) {
            var r: tsyn_eVoteReq; 
            if (input.dest == Node1) {
                cache += ((controller = input.controller, dst = nodes, src = Node1, dest = Node2, leader = Node1));
                // do_send_vote_req((controller = input.controller, dst = nodes, src = Node1, dest = Node2, leader = Node1));
            } else {
                do_send_vote_req((controller = input.controller, dst = nodes, src = Node2, dest = Node1, leader = Node2));
                foreach(r in cache) {
                    do_send_vote_req(r);
                }
                cache = default(set[tsyn_eVoteReq]);
            }
        }
        on syn_eVoteReq do (input: tsyn_eVoteReq) {
            if (input.dest == Node1) {
                send input.controller, syn_eVoteRsp, (controller = input.controller, dst = nodes, src = Node1, dest = Node2, stat = true);
            } else {
                send input.controller, syn_eVoteRsp, (controller = input.controller, dst = nodes, src = Node2, dest = Node1, stat = false);
            }
        }
        on syn_eVoteRsp do (input: tsyn_eVoteRsp) {
            if (!(input.dest in leaders)) {
                send input.controller, syn_eBecomeLeader, (controller = input.controller, dst = nodes, leader = input.dest);
            }
            leaders += (input.dest);
        }
        ignore syn_eBecomeLeader;
    }
    fun do_send_vote_req(input: tsyn_eVoteReq) {
        send input.controller, syn_eVoteReq, input;
    }
}

// machine Proposer {
//     var acceptorMap: map[tAcceptorNode, machine];
//     var leaner: machine;
//     var lostPrReq: set [(a: tAcceptorNode, p: tProposerNode)];
//     var lostAcReq: set [(a: tAcceptorNode, p: tProposerNode)];
//     start state Init {
//         entry (ln: machine) {
//             leaner = ln;
//             receive { 
//                 case eInit: (input: setting){
//                     acceptorMap = input.acceptorMap;
//                 }
//             }
//             goto Loop;
//         }
//     }
//     state Loop {
//         entry {
//             // var i: int;
//             // if (sizeof(cache) >= 2) {
//             //     i = sizeof(cache) - 1;
//             //     while(i >= 0) {
//             //         do_send_write_to_tail(cache[i]);
//             //         i = i - 1;
//             //     }
//             //     cache = default(seq[tsyn_writeToTail]);
//             // }
//         }
//         on syn_eStart do (input: tsyn_eStart) {
//             unreadlizablePrepareReq((controller = input.controller, dst = acceptorMap[Acceptor1], proposer = input.proposer, acceptor = Acceptor1, va = input.va));
//             unreadlizablePrepareReq((controller = input.controller, dst = acceptorMap[Acceptor2], proposer = input.proposer, acceptor = Acceptor2, va = input.va));
//             goto Loop;
//         }
//         on syn_eLostPrepareReq do (input: tsyn_eLostPrepareReq) {
//             lostPrReq += ((a = input.acceptor, p = input.proposer));
//             goto Loop;
//         }
//         on syn_eLostAcceptReq do (input: tsyn_eLostAcceptReq) {
//             lostAcReq += ((a = input.acceptor, p = input.proposer));
//             goto Loop;
//         } 
//         on syn_ePrepareRsp do (input: tsyn_ePrepareRsp) {
//             unreadlizableAcceptReq((controller = input.controller, dst = acceptorMap[input.acceptor], proposer = input.promised, acceptor = input.acceptor, va = input.va));
//             goto Loop;
//         }
//         on syn_eAcceptRsp do (input: tsyn_eAcceptRsp) {
//             send input.controller, syn_eLearn, (controller = input.controller, dst = leaner, va = input.va);
//             goto Loop;
//         }
//     }
//     fun unreadlizablePrepareReq (input: tsyn_ePrepareReq) {
//         var pair: (a: tAcceptorNode, p: tProposerNode);
//         pair.a = input.acceptor;
//         pair.p = input.proposer;
//         if (!(pair in lostPrReq)) {
//             send input.controller, syn_ePrepareReq, input;
//         }
//     }
//     fun unreadlizableAcceptReq (input: tsyn_eAcceptReq) {
//         var pair: (a: tAcceptorNode, p: tProposerNode);
//         pair.a = input.acceptor;
//         pair.p = input.proposer;
//         if (!(pair in lostAcReq)) {
//             send input.controller, syn_eAcceptReq, input;
//         }
//     }
// }

// machine Acceptor {
//     var proposerMap: map[tProposerNode, machine];
//     var lostPrRsp: set [(a: tAcceptorNode, p: tProposerNode)];
//     var lostAcRsp: set [(a: tAcceptorNode, p: tProposerNode)];
//     var cache: set [tsyn_ePrepareRsp];
//     start state Init {
//         entry {
//             receive { 
//                 case eInit: (input: setting){
//                     proposerMap = input.proposerMap;
//                 }
//             }
//             goto Loop;
//         }
//     }
//     state Loop {
//         entry {
//             var i: int;
//             // if (sizeof(cache) >= 2) {
//             //     i = sizeof(cache) - 1;
//             //     while(i >= 0) {
//             //         do_send_write_to_tail(cache[i]);
//             //         i = i - 1;
//             //     }
//             //     cache = default(seq[tsyn_writeToTail]);
//             // }
//         }
//         on syn_eLostPrepareRsp do (input: tsyn_eLostPrepareRsp) {
//             lostPrRsp += ((a = input.acceptor, p = input.promised));
//             goto Loop;
//         }
//         on syn_eLostAcceptRsp do (input: tsyn_eLostAcceptRsp) {
//             lostAcRsp += ((a = input.acceptor, p = input.proposer));
//             goto Loop;
//         } 
//         on syn_ePrepareReq do (input: tsyn_ePrepareReq) {
//             unreadlizablePrepareRsp((controller = input.controller, dst = proposerMap[input.proposer], acceptor = input.acceptor, promised = input.proposer, va = input.va, n_accepted = input.proposer));
//             goto Loop;
//         }
//         on syn_eAcceptReq do (input: tsyn_eAcceptReq) {
//             unreadlizableAcceptRsp((controller = input.controller, dst = proposerMap[input.proposer], proposer = input.proposer, acceptor = input.acceptor, accepted = input.proposer, va = input.va));
//             goto Loop;
//         }
//     }
//     fun unreadlizablePrepareRsp (input: tsyn_ePrepareRsp) {
//         var pair: (a: tAcceptorNode, p: tProposerNode);
//         pair.a = input.acceptor;
//         pair.p = input.promised;
//         if (!(pair in lostPrRsp)) {
//             send input.controller, syn_ePrepareRsp, input;
//         }
//     }
//     fun unreadlizableAcceptRsp (input: tsyn_eAcceptRsp) {
//         var pair: (a: tAcceptorNode, p: tProposerNode);
//         pair.a = input.acceptor;
//         pair.p = input.proposer;
//         if (!(pair in lostAcRsp)) {
//             send input.controller, syn_eAcceptRsp, input;
//         }
//     }
// }
