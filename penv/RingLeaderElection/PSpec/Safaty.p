event an_syn_eNominate: tsyn_eNominate;
event an_syn_eWon: tsyn_eWon;

spec unique_leader observes an_syn_eWon {
  var has_leader: bool;
  var leader: tNode;
  start state Init {
    entry{
      has_leader = false;
    }
    on an_syn_eWon do (input: tsyn_eWon) {
      if (has_leader) {
        assert (leader == input.leader), "property violation";
      } else {
        has_leader = true;
        leader = input.leader;
      }
    }
  }
}