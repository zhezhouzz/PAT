enum tNode {Node1, Node2}
type setting = machine;
type tsyn_eStart = (controller:machine, dst:machine, node:tNode);
type tsyn_eInternalReq = (controller:machine, dst:machine, node:tNode);
type tsyn_eForwardReq = (controller:machine, dst:machine, node:tNode);
type tsyn_eExternalRsp = (controller:machine, dst:machine, node:tNode, stat:bool);
type tsyn_eExternalReq = (controller:machine, dst:machine, node:tNode);
event syn_eStart: tsyn_eStart;
event syn_eInternalReq: tsyn_eInternalReq;
event syn_eForwardReq: tsyn_eForwardReq;
event syn_eExternalRsp: tsyn_eExternalRsp;
event syn_eExternalReq: tsyn_eExternalReq;
event connectFW: (fw: machine);