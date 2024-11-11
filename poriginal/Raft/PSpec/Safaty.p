spec leanerConsistentView observes syn_eBecomeLeader, syn_eAppendEntry {
  var store: map[tNode, set[tVal]];
  var has_leader: bool; 
  var leader: tNode;
  var finalView: set[tVal];
  start state Init {
    entry{
      store[Node1] = default(set[tVal]);
      store[Node2] = default(set[tVal]);
    }
    on syn_eAppendEntry do (input: tsyn_eAppendEntry) {
      store[input.node] += (input.va);
    }
    on syn_eBecomeLeader do (input: tsyn_eBecomeLeader) {
      var i: tVal;
      if(has_leader) {
        assert (leader == input.leader), "property violation";  
      }
      has_leader = true;
      leader = input.leader;
      finalView = store[input.leader]; 
    }
  }
}