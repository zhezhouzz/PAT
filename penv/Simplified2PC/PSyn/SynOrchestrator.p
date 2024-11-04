machine SynOrchestrator {
  start state Init {
    entry {
      var database: machine;
      var coordinator: machine;
      var domain_int: set[int];
      var domain_bool: set[bool];
      domain_int += (-1);
      domain_int += (0);
      domain_int += (1);
      domain_bool += (true);
      domain_bool += (false);
      database = new Database();
      coordinator = new Coordinator((database = database, ));
      new SynClient((setting = coordinator, domain_int = domain_int, domain_bool = domain_bool));
    }
  }
}

module Server = { Database, Coordinator, Timer};

test Syn [main=SynOrchestrator]:
  assert strong_consistenty in (union { SynOrchestrator, SynClient}, Server);
