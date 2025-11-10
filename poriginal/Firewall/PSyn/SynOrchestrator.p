machine SynOrchestrator {
    start state Init {
      entry {
        var internal: machine;
        var external: machine;
        var fw: machine;
        var domain_tNode: set[tNode];
        var domain_bool: set[bool];
        domain_tNode += (Node1);
        domain_tNode += (Node2);
        domain_bool += (true);
        domain_bool += (false);
        internal = new Internal();
        external = new External();
        fw = new Firewall((internal = internal, external = external));
        send internal, connectFW, (fw = fw,);
        send external, connectFW, (fw = fw,);
        new SynClient((setting = internal, domain_bool = domain_bool, domain_tNode = domain_tNode));
      }
    }
  }
  
  module M = { SynOrchestrator, SynClient, Internal, External, Firewall, Timer};
  
  test Syn [main=SynOrchestrator]: assert allow_all_session_from_internal_node in M;