type tsyn_writeRsp = (controller:machine, dst:machine, va:int);
type tsyn_writeReq = (controller:machine, dst:machine, va:int);
type tsyn_readRsp = (controller:machine, dst:machine, va:int, st:bool);
type tsyn_readReq = (controller:machine, dst:machine);
event syn_writeRsp: tsyn_writeRsp;
event syn_writeReq: tsyn_writeReq;
event syn_readRsp: tsyn_readRsp;
event syn_readReq: tsyn_readReq;
type setting = machine;