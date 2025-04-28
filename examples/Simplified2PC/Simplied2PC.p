event readReq: (src: int, dst: int);
event getReq: (src: int, dst: int);
event readRsp: (src: int, dst: int, va: int);
event writeReq: (src: int, dst: int, va: int);
event putReq: (src: int, dst: int, va: int);
event putRsp: (src: int, dst: int, va: int);
event writeRsp: (src: int, dst: int, va: int, stat: bool);
event commit: (src: int, dst: int);
event abort: (src: int, dst: int);

pat readReq (tCoordinator <: int, tDatabase <: int) with (input: (src: int, dst: tCoordinator)) {
    history = true;
    current = true;
    future = exists e: getReq. e.src == input.dst && e.dst: tDatabase;
}

pat getReq with (input: (src: int, dst: int), gva: int) {
    history = forall e: Commit. false;
    current = true;
    future = exists e: ReadRsp. e.src == input.dst && e.dst == generator && e.va == -1;
} && {
    history = exists e1: putReq. exists e2: Commit. after(e1, e2) && e1.va == gva;
    current = true;
    future = exists e: ReadRsp. e.src == input.dst && e.dst == generator && e.va == gva;
}

pat readRsp;

pat writeReq (tCoordinator <: int, tDatabase <: int) with (input: (src: int, dst: tCoordinator, va: int)) {
    history = true;
    current = true;
    future = exists e: putReq. e.src == input.dst && e.dst: tDatabase && va == input.va;
}

pat putReq with (input: (src: int, dst: int, va: int)) {
    history = true;
    current = true;
    future = exists e: PutRsp. e.src == input.dst && e.dst == input.src && va == input.va;
}

pat putRsp with (input: (src: int, dst: int, va: int, stat: bool)) {
    history = true;
    current = stat == false;
    future = exists e1: WriteRsp. exists e2: Commit. e1.src == input.dst && e1.dst == generator && e1.va == input.va && e1.stat = input.stat && e2.src == input.dst && e2.dst == input.src;
} && {
    history = exists e1: putReq. exists e2: Commit. after(e1, e2) && e1.va == gva;
    current = stat == true;
    future = exists e1: WriteRsp. exists e2: Abort. e1.src == input.dst && e1.dst == generator && e1.va == input.va && e1.stat = input.stat && e2.src == input.dst && e2.dst == input.src;
}

pat commit;
pat abort;
pat writeRsp;

prop neg_readAfterWrite = 
    exists e1: WriteRsp. exists e2: ReadRsp. after(e1, e2) && e1.stat == true && e2.va != e1.va && 
        (forall e3: WriteRsp. after(e1, e3) && after(e3, e2) && e3.va != e2.va && e3.stat == true)
// There is not an WriteRsp with valid status between two events

syn machine SynClient (input: (coordinators: set[machine], databases: set[machine])) = {
    gen = [readReq input.coordinators input.databases, writeReq input.coordinators input.databases];
    obs = [readRsp, writeRsp];
    hidden = [getReq, putReq, putRsp, commit, abort];
    props = [neg_readAfterWrite]
}