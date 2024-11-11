spec leanerConsistentView observes syn_eLearn {
  var store: int;
  var is_init: bool;
  start state Init {
    entry{
      is_init = false;
    }
    on syn_eLearn do (input: tsyn_eLearn) {
      if (is_init) {
        assert (input.va == store), "property violation";  
      } else {
        is_init = true;
        store = input.va;
      }
    }
  }
}