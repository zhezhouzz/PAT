machine SynOrchestrator {
    start state Init {
      entry {
        var head: machine;
        var mid: machine;
        var tail: machine;
        var domain_tNode: set[tNode];
        var domain_bool: set[bool];
        var domain_int: set[int];
        var domain_tKey: set[tKey];
        domain_tNode += (Node1);
        domain_tNode += (Node2);
        domain_bool += (true);
        domain_bool += (false);
        domain_tKey += (0);
        domain_tKey += (1);
        domain_tKey += (2);
        domain_tKey += (3);
        domain_tKey += (4);
        domain_tKey += (5);
        domain_tKey += (6);
        domain_tKey += (7);
        domain_tKey += (8);
        domain_tKey += (9);
        domain_tKey += (10);
        domain_int += (2);
        domain_int += (3);
        tail = new Tail();
        mid = new Mid(tail);
        head = new Head(mid);
        send tail, mkChain, (mid = mid,);
        new SynClient((setting = (head = head, mid = mid, tail = tail), domain_int = domain_int, domain_bool = domain_bool, domain_tKey = domain_tKey, domain_tNode = domain_tNode));
      }
    }
  }
  
  module M = { SynOrchestrator, SynClient, Head, Mid, Tail, Timer};
  
  test Syn [main=SynOrchestrator]: assert ryw in M;