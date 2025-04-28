// Each machine has an unique and comparable id
// Generator always has id 0
event eWakeup: (src: int, dst: int);
event eNominate: (src: int, dst: int, leader: int);
event eWon: (src: int, dst: int, leader: int);

// We define the topological structure of nodes
fun next (tNode: set[int]) with (m: tNode) = {
    if ((m + 1) in tNode) {
        return m + 1;   
    }
    return 1;
}

// wakeup will trigger a enomiate with leader as itself.
pat eWakeup (tNode <: int) with (input: (src: int, dst: tNode)) {
    history = forall e: eWon. false;
    current = true;
    future = exists e: eNominate. e.src == input.dst && e.dst == next(tNode)(input.dst) && leader == input.dst;
}

// enomiate with leader as itself.
pat eNominate (tNode <: int) with (input: (src: tNode, dst: tNode, leader: tNode)) {
    history = true;
    current = input.leader == input.dst;
    future = exists e: eWon. e.src == input.dst && e.dst == 0 && leader == input.dst;
} and {
    history = true;
    current = input.leader != input.dst;
    future = exists e: eNominate. e.src == input.dst && e.dst == next(tNode)(input.dst) && leader == input.leader;
}

pat eWon;

prop non_uniqueLeader = exists e1 e2: eWon. e1.leader != e2.leader

syn machine SynClient (input: (nodes: set[machine])) = {
    gen = [eWakeup input.nodes];
    obs = [eWon];
    hidden = [eNominate input.nodes];
    props = [on_uniqueLeader]
}