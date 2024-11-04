fun send_eEspressoButtonPressed (src: machine, dest: machine) {
    send dest, syn_eEspressoButtonPressed, (controller = src, dst = dest);
  }

fun send_eCoffeeMachineUser (src: machine, dest: machine) {
    send dest, syn_eCoffeeMachineUser, (controller = src, dst = dest);
  }

fun cast_syn_eCoffeeMakerError (input: tsyn_eCoffeeMakerError): (st: tCoffeeMakerState) {
    return (st = input.st,);
  }

fun forward_syn_eWarmUpReq (input: tsyn_eWarmUpReq) {
    send input.dst, syn_eWarmUpReq, input;
  }

fun forward_syn_eWarmUpCompleted (input: tsyn_eWarmUpCompleted) {
    send input.dst, syn_eWarmUpCompleted, input;
  }

fun forward_syn_eStartEspressoReq (input: tsyn_eStartEspressoReq) {
    send input.dst, syn_eStartEspressoReq, input;
  }

fun forward_syn_eNoWaterError (input: tsyn_eNoWaterError) {
    send input.dst, syn_eNoWaterError, input;
  }

fun forward_syn_eNoBeansError (input: tsyn_eNoBeansError) {
    send input.dst, syn_eNoBeansError, input;
  }

fun forward_syn_eGrindBeansReq (input: tsyn_eGrindBeansReq) {
    send input.dst, syn_eGrindBeansReq, input;
  }

fun forward_syn_eGrindBeansCompleted (input: tsyn_eGrindBeansCompleted) {
    send input.dst, syn_eGrindBeansCompleted, input;
  }

fun forward_syn_eEspressoCompleted (input: tsyn_eEspressoCompleted) {
    send input.dst, syn_eEspressoCompleted, input;
  }

fun forward_syn_eEspressoButtonPressed (input: tsyn_eEspressoButtonPressed) {
    send input.dst, syn_eEspressoButtonPressed, input;
  }

fun forward_syn_eCoffeeMakerReady (input: tsyn_eCoffeeMakerReady) {
    send input.dst, syn_eCoffeeMakerReady, input;
  }

fun forward_syn_eCoffeeMakerError (input: tsyn_eCoffeeMakerError) {
    send input.dst, syn_eCoffeeMakerError, input;
  }

fun forward_syn_eCoffeeMakerCompleted (input: tsyn_eCoffeeMakerCompleted) {
    send input.dst, syn_eCoffeeMakerCompleted, input;
  }

fun forward_syn_eCoffeeMachineUser (input: tsyn_eCoffeeMachineUser) {
    send input.dst, syn_eCoffeeMachineUser, input;
  }
