event readReq;
event readRsp: (va: int);
event writeReq: (va: int);
event writeRsp: (va: int, stat: bool);

// Temporal Relation
constraint c0 { readReq -> readRsp }
constraint c1 { writeReq(va) -> writeRsp(va) }

// Payload Constraints
constraint c2(values) as writeReq when {
    // 1. the payload of writeReq should be in the given set of values
    // 2. the payload of writeReq is not equal to any previous payload of writeReq
    self.va in values &&
    forall e1:writeReq. after(e1, self) ==> e1.va != self.va
}

constraint c3 as readRsp when {
    // 1. the payload of readRsp is equal to one of provious written value
    exists e1:writeReq. after(e1, self) && e1.va == self.va
}

prop neg_readAfterWrite = 
    exists e1: WriteRsp. exists e2: ReadRsp. after(e1, e2) && e1.stat == true && e2.va != e1.va && 
        (forall e3: WriteRsp. after(e1, e3) && after(e3, e2) && e3.va != e2.va && e3.stat == true)

syn machine SynClient (input: (values: set[int])) = {
    gen = [readReq writeReq];
    obs = [readRsp, writeRsp];
    constriants = [c0, c1, c2(values), c3];
    props = [neg_readAfterWrite]
}