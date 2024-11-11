machine SynOrchestrator {
    start state Init {
      entry {
        var db: machine;
        var bank: machine;
        var domain_int: set[int];
        var domain_aid: set[int];
        var domain_rid: set[int];
        var domain_bool: set[bool];
        db = new Database();
        bank = new Bank(db);
        domain_aid += (0);
        domain_aid += (1);
        domain_aid += (2);
        domain_aid += (3);
        domain_rid += (0);
        domain_rid += (1);
        domain_int += (0);
        domain_int += (1);
        domain_int += (2);
        domain_int += (3);
        domain_int += (4);
        domain_int += (5);
        domain_int += (6);
        domain_int += (7);
        domain_int += (8);
        domain_int += (9);
        domain_bool += (true);
        domain_bool += (false);
        new SynClient((setting = bank, domain_int = domain_int, domain_bool = domain_bool, domain_aid = domain_aid, domain_rid = domain_rid));
      }
    }
  }
  
  module M = { SynOrchestrator, SynClient, Database, Bank, Timer};
  
  test Syn [main=SynOrchestrator]: assert bank_safe in M;