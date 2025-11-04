machine SynOrchestrator {
    start state Init {
      entry {
        var head: machine;
        var mid: machine;
        var tail: machine;
        var domain_tAcceptorNode: set[tAcceptorNode];
        var domain_tProposerNode: set[tProposerNode];
        var domain_tVal: set[tVal];
        var leaner: machine;
        var proposerMap: map[tProposerNode, machine];
        var acceptorMap: map[tAcceptorNode, machine];
        var setting: setting;
        domain_tAcceptorNode += (Acceptor1);
        domain_tAcceptorNode += (Acceptor2);
        domain_tProposerNode += (Proposer1);
        domain_tProposerNode += (Proposer2);
        domain_tVal += (0);
        domain_tVal += (1);
        leaner = new Learner();
        proposerMap[Proposer1] = new Proposer(leaner);
        proposerMap[Proposer2] = new Proposer(leaner);
        acceptorMap[Acceptor1] = new Acceptor();
        acceptorMap[Acceptor2] = new Acceptor();
        setting = (proposerMap = proposerMap, acceptorMap = acceptorMap);
        send proposerMap[Proposer1], eInit, setting;
        send proposerMap[Proposer2], eInit, setting;
        send acceptorMap[Acceptor1], eInit, setting;
        send acceptorMap[Acceptor2], eInit, setting;
        new SynClient((setting = setting, domain_tAcceptorNode = domain_tAcceptorNode, domain_tProposerNode = domain_tProposerNode, domain_tVal = domain_tVal));
      }
    }
  }
  
  module M = { SynOrchestrator, SynClient, Proposer, Acceptor, Learner, Timer};
  
  test Syn [main=SynOrchestrator]: assert leanerConsistentView in M;