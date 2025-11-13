machine Node {
    var nodeRing: map[tNode, machine];
    var selfNode: tNode;
    var nextNode: tNode;
    var controller: machine;
    start state Init {
        entry (input: (selfNode: tNode, nextNode: tNode)){
            selfNode = input.selfNode;
            nextNode = input.nextNode;
            receive {
                case initRing: (input: tinitRing) {
                    nodeRing = input.nodeRing;
                }
            }
        }

        on syn_eWakeup do (input: tsyn_eWakeup) {
            send input.controller, syn_eNominate, (controller = input.controller, dst = nodeRing[nextNode], node = selfNode, leader = selfNode);
        }


        on syn_eNominate do (input: tsyn_eNominate) {
            if (input.leader == selfNode) {
                send input.controller, syn_eWon, (controller = input.controller, dst = input.controller, leader = selfNode);
            } else {
                send input.controller, syn_eNominate, (controller = input.controller, dst = nodeRing[nextNode], node = selfNode, leader = input.leader);
            }
        }
    }
}