/* Events used to inform monitor about the internal state of the CoffeeMaker */
event eInWarmUpState;
event eInReadyState;
event eInBeansGrindingState;
event eInCoffeeBrewingState;
event eErrorHappened;
event eResetPerformed;

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