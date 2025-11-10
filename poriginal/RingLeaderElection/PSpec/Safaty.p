spec unique_leader observes syn_eWon {
  var has_leader: bool;
  var leader: tNode;
  start state Init {
    entry{
      has_leader = false;
    }
    on syn_eWon do (input: tsyn_eWon) {
      if (has_leader) {
        assert (leader == input.leader), "property violation";
      } else {
        has_leader = true;
        leader = input.leader;
      }
    }
  }
}