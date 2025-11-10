machine SynOrchestrator {
    start state Init {
      entry {
        var db: machine;
        var domain_int: set[int];
        var domain_bool: set[bool];
        db = new Database();
        domain_int += (0);
        domain_int += (1);
        domain_bool += (true);
        domain_bool += (false);
        new SynClient((setting = db, domain_int = domain_int, domain_bool = domain_bool));
      }
    }
  }
  
  module M = { SynOrchestrator, SynClient, Database, Timer};
  
  test Syn [main=SynOrchestrator]: assert ryw in M;