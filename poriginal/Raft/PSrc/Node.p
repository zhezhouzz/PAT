machine InitNode {
    var nodes: machine;
    var store: int;
    // var timer: Timer;
    start state Init {
        entry {
            receive {                 
                case eInit: (input: setting){                
                    nodes = input.nodes;
                }
            }
        }
        on syn_eStart do (input: tsyn_eStart) {
        }
        on syn_eClientPut do (input: tsyn_eClientPut) {
            RealSend(nodes, syn_eAppendEntry, (controller = input.controller, dst = nodes, node = Node2, va = input.va));
            RealSend(nodes, syn_eAppendEntry, (controller = input.controller, dst = nodes, node = Node1, va = input.va));
        }
        on syn_eShutDown do (input: tsyn_eShutDown) {
            RealSend(nodes, syn_eTimeout, (controller = input.controller, dst = nodes, dest = Node2));
            RealSend(nodes, syn_eTimeout, (controller = input.controller, dst = nodes, dest = Node1));
            raise halt;
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
                RealSend(nodes, syn_eVoteRsp, (controller = input.controller, dst = nodes, src = Node1, dest = Node2, stat = true));
            } else {
                RealSend(nodes, syn_eVoteRsp, (controller = input.controller, dst = nodes, src = Node2, dest = Node1, stat = false));
            }
        }
        on syn_eVoteRsp do (input: tsyn_eVoteRsp) {
            if (!(input.dest in leaders)) {
                RealSend(nodes, syn_eBecomeLeader, (controller = input.controller, dst = nodes, leader = input.dest));
            }
            leaders += (input.dest);
        }
        on syn_eShutDown do (input: tsyn_eShutDown) {
            raise halt;
        }
        ignore syn_eBecomeLeader;
    }
    fun do_send_vote_req(input: tsyn_eVoteReq) {
        send input.dst, syn_eVoteReq, input;
    }
}
