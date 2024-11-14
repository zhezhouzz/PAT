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

test Syn [main=SynOrchestrator]:
  assert no_water_error in (union { SynOrchestrator, SynClient }, EspressoMachine);

// test tc_no_beans_error [main=SynOrchestrator]:
//   assert no_beans_error in (union { SynOrchestrator, SynClient }, EspressoMachine);