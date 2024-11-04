machine SynOrchestrator {
    start state Init {
      entry {
        var node1: machine;
        var node2: machine;
        var domain_tNode: set[tNode];
        var ring: map[tNode, machine];
        domain_tNode += (Node1);
        domain_tNode += (Node2);
        node1 = new Node((selfNode = Node1, nextNode = Node2));
        node2 = new Node((selfNode = Node2, nextNode = Node1));
        ring[Node1] = node1;
        ring[Node2] = node2;
        send node1, initRing, (nodeRing = ring,);
        send node2, initRing, (nodeRing = ring,);
        new SynClient((setting = ring, domain_tNode = domain_tNode));
      }
    }
  }
  
  module M = { SynOrchestrator, SynClient, Node, Timer};
  
  test Syn [main=SynOrchestrator]: assert unique_leader in M;