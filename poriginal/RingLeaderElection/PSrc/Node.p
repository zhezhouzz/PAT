machine Node {
    var nodeRing: map[tNode, machine];
    var selfNode: tNode;
    var nextNode: tNode;
    var timer: Timer;
    var controller: machine;
    var cache: tsyn_eNominate; 
    start state Init {
        entry (input: (selfNode: tNode, nextNode: tNode)){
            selfNode = input.selfNode;
            nextNode = input.nextNode;
            timer = CreateTimer(this);
            receive {
                case initRing: (input: tinitRing) {
                    nodeRing = input.nodeRing;
                }
            }
        }

        on syn_eWakeup do (input: tsyn_eWakeup) {
            if (selfNode == Node1) {
                controller = input.controller;
                cache = (controller = input.controller, dst = nodeRing[nextNode], node = selfNode, leader = selfNode);
                StartTimer(timer);
            } else {
                RealSend(nodeRing[nextNode], syn_eNominate, (controller = input.controller, dst = nodeRing[nextNode], node = selfNode, leader = selfNode));
            }
        }

        on eTimeOut do {
            RealSend(cache.dst, syn_eNominate, cache);
        }

        on syn_eNominate do (input: tsyn_eNominate) {
            if (input.leader == selfNode) {
                RealSend(input.controller, syn_eWon, (controller = input.controller, dst = input.controller, leader = selfNode));
            } else {
                RealSend(nodeRing[nextNode], syn_eNominate, (controller = input.controller, dst = nodeRing[nextNode], node = selfNode, leader = input.leader));
            }
        }
    }
}