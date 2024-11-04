enum tNode {Node1, Node2}
type setting = (head: machine, mid: machine, tail: machine);
type tKey = int;
type tsyn_writeToTail = (controller:machine, dst:machine, key:tKey, va:int);
type tsyn_writeToMid = (controller:machine, dst:machine, key:tKey, va:int, node:tNode);
type tsyn_writeRsp = (controller:machine, dst:machine, key:tKey, va:int);
type tsyn_writeReq = (controller:machine, dst:machine, key:tKey, va:int);
type tsyn_readRsp = (controller:machine, dst:machine, key:tKey, va:int, st:bool);
type tsyn_readReq = (controller:machine, dst:machine, key:tKey);
type tsyn_crashTail = (controller:machine, dst:machine);
event syn_writeToTail: tsyn_writeToTail;
event syn_writeToMid: tsyn_writeToMid;
event syn_writeRsp: tsyn_writeRsp;
event syn_writeReq: tsyn_writeReq;
event syn_readRsp: tsyn_readRsp;
event syn_readReq: tsyn_readReq;
event syn_crashTail: tsyn_crashTail;
event mkChain: (mid: machine);