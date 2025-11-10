/* Events used to inform monitor about the internal state of the CoffeeMaker */
event eInWarmUpState;
event eInReadyState;
event eInBeansGrindingState;
event eInCoffeeBrewingState;
event eErrorHappened;
event eResetPerformed;

event an_syn_eCoffeeMakerError: tsyn_eCoffeeMakerError;
event an_syn_eCoffeeMakerCompleted: tsyn_eCoffeeMakerCompleted;
event an_syn_eCoffeeMakerReady: tsyn_eCoffeeMakerReady;
event an_syn_eCoffeeMachineUser: tsyn_eCoffeeMachineUser;
event an_syn_eWarmUpReq: tsyn_eWarmUpReq;
event an_syn_eWarmUpCompleted: tsyn_eWarmUpCompleted;
event an_syn_eStartEspressoReq: tsyn_eStartEspressoReq;
event an_syn_eNoWaterError: tsyn_eNoWaterError;
event an_syn_eNoBeansError: tsyn_eNoBeansError;
event an_syn_eGrindBeansReq: tsyn_eGrindBeansReq;
event an_syn_eGrindBeansCompleted: tsyn_eGrindBeansCompleted;
event an_syn_eEspressoCompleted: tsyn_eEspressoCompleted;
event an_syn_eEspressoButtonPressed: tsyn_eEspressoButtonPressed;


spec no_water_error
observes syn_eCoffeeMakerError
{
  start state StartUp {
    on syn_eCoffeeMakerError do (input: tsyn_eCoffeeMakerError) {
      if (input.st == NoWaterError) {
        assert false, "spec violation";
      } 
    }
  }
}

spec no_beans_error
observes syn_eCoffeeMakerError
{
  start state StartUp {
    on syn_eCoffeeMakerError do (input: tsyn_eCoffeeMakerError) {
      if (input.st == NoBeansError) {
        assert false, "spec violation";
      } 
    }
  }
}