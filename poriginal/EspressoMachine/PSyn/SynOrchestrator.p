machine SynOrchestrator {
  start state Init {
    entry {
      var setting: machine;
      var domain_tCoffeeMakerState: set[tCoffeeMakerState];
      domain_tCoffeeMakerState += (NotWarmedUp);
      domain_tCoffeeMakerState += (Ready);
      domain_tCoffeeMakerState += (NoBeansError);
      domain_tCoffeeMakerState += (NoWaterError);
      setting = new CoffeeMakerControlPanel();
      new SynClient((setting = setting, domain_tCoffeeMakerState = domain_tCoffeeMakerState));
    }
  }
}

machine Orchestrator {
  start state Init {
    entry {
      var setting: machine;
      var domain_tCoffeeMakerState: set[tCoffeeMakerState];
      domain_tCoffeeMakerState += (NotWarmedUp);
      domain_tCoffeeMakerState += (Ready);
      domain_tCoffeeMakerState += (NoBeansError);
      domain_tCoffeeMakerState += (NoWaterError);
      setting = new CoffeeMakerControlPanel();
      new Client((setting = setting, domain_tCoffeeMakerState = domain_tCoffeeMakerState));
    }
  }
}

test Syn [main=SynOrchestrator]:
  assert no_water_error in (union { SynOrchestrator, SynClient, Timer }, EspressoMachine);

test Manual [main=Orchestrator]:
  assert no_water_error in (union { Orchestrator, Client, Timer }, EspressoMachine);