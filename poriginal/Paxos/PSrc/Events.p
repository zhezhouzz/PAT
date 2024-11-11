enum tProposerNode {Proposer1, Proposer2}
enum tAcceptorNode {Acceptor1, Acceptor2}
type setting = (proposerMap: map[tProposerNode, machine], acceptorMap: map[tAcceptorNode, machine]);
type tVal = int;
type tsyn_eStart = (controller:machine, dst:machine, proposer:tProposerNode, va:tVal);
type tsyn_ePrepareRsp = (controller:machine, dst:machine, acceptor:tAcceptorNode, promised:tProposerNode, va:tVal, n_accepted:tProposerNode);
type tsyn_ePrepareReq = (controller:machine, dst:machine, proposer:tProposerNode, acceptor:tAcceptorNode, va:tVal);
type tsyn_eLostPrepareRsp = (controller:machine, dst:machine, acceptor:tAcceptorNode, promised:tProposerNode);
type tsyn_eLostPrepareReq = (controller:machine, dst:machine, proposer:tProposerNode, acceptor:tAcceptorNode);
type tsyn_eLostAcceptRsp = (controller:machine, dst:machine, proposer:tProposerNode, acceptor:tAcceptorNode);
type tsyn_eLostAcceptReq = (controller:machine, dst:machine, proposer:tProposerNode, acceptor:tAcceptorNode);
type tsyn_eLearn = (controller:machine, dst:machine, va:tVal);
type tsyn_eAcceptRsp = (controller:machine, dst:machine, proposer:tProposerNode, acceptor:tAcceptorNode, accepted:tProposerNode, va:tVal);
type tsyn_eAcceptReq = (controller:machine, dst:machine, proposer:tProposerNode, acceptor:tAcceptorNode, va:tVal);
event syn_eStart: tsyn_eStart;
event syn_ePrepareRsp: tsyn_ePrepareRsp;
event syn_ePrepareReq: tsyn_ePrepareReq;
event syn_eLostPrepareRsp: tsyn_eLostPrepareRsp;
event syn_eLostPrepareReq: tsyn_eLostPrepareReq;
event syn_eLostAcceptRsp: tsyn_eLostAcceptRsp;
event syn_eLostAcceptReq: tsyn_eLostAcceptReq;
event syn_eLearn: tsyn_eLearn;
event syn_eAcceptRsp: tsyn_eAcceptRsp;
event syn_eAcceptReq: tsyn_eAcceptReq;
event eInit: setting;