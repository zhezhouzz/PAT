enum tNode {Node1, Node2} 
type setting = map[tNode, machine];
type tsyn_eWon = (controller:machine, dst:machine, leader:tNode);
type tsyn_eWakeup = (controller:machine, dst:machine, node:tNode);
type tsyn_eNominate = (controller:machine, dst:machine, node:tNode, leader:tNode);
type tinitRing = (nodeRing: map[tNode, machine]);
event syn_eWon: tsyn_eWon;
event syn_eWakeup: tsyn_eWakeup;
event syn_eNominate: tsyn_eNominate;
event initRing: tinitRing;