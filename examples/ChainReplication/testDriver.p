type tKVMsg = (src: machine, k: int, v: int, id: int);

event eWriteRequest  : tKVMsg;
event eWriteResponse : tKVMsg;

event eReadRequest   : tKVMsg;
event eReadResponse  : (status: bool, kvmsg: tKVMsg);

event eShutDown: (id: int);

param chainSize: int;

fun isHead(nodeId: int): bool {
    assert (chainSize > 0);
    return nodeId == 0;
}

fun isTail(nodeId: int): bool {
    assert (chainSize > 0);
    return nodeId == (chainSize - 1);
}

fun isMid(nodeId: int): bool {
    assert (chainSize > 2);
    return (0 < nodeId && nodeId < chainSize);
}

temporal T1 = eWriteRequest -> eWriteResponse
temporal T2 = eReadRequest -> eReadResponse

global strongConsistency = 
    // For all read responses
    forall eR: eReadResponse, eR.status => 
        // Exists a previous write response with the same value
        (exists eW: eWriteResponse, after(eW, eR) && eR.kvmsg.k == eW.k && eR.kvmsg.v == eW.v) &&
        // All other write response with diffrent value are not the last one
        (forall eWOther: eWriteResponse, after(eWOther, eR) && eR.kvmsg.k == eW.k && eR.kvmsg.v != eW.v
            exists eWLater: eWriteResponse, after(eWOther, eWLater) && after(eWLater, eR))

payload writeHead =
    on e: eWriteRequest with
        forall i: int, 0 <= i && i < e.id => exists e: eShutDown. e.id == i

payload readTail =
    on e: eReadRequest with
        forall i: int, e.id < i && i < chainSize => exists e: eShutDown. e.id == i

payload crashHead =
    on e: eShutDown with
    exists eW: eWriteRequest,
        after (eW, e) && isHead(eW.id) && e.dst == eW.id &&
        (forall eW2: eWriteRequest, after(eW, eW2) && after(eW2, e) => eW2.src != e.dst)

payload crashTail =
    on e: eShutDown with
    exists eW: eWriteRequest,
        after (eW, e) && isTail(eW.id) && e.dst == eW.id &&
        (forall eW2: eWriteResponse, after(eW, eW2) && after(eW2, e) => eW2.src != e.dst)

payload crashMid =
    on e: eShutDown with
    exists eW: eWriteRequest,
        after (eW, e) && isMid(eW.id) && e.dst == eW.id &&
        (forall eW2: eWriteRequest, after(eW, eW2) && after(eW2, e) => eW2.src != e.dst)

payload uniqueWriteValue =
    on e: eWriteRequest with
    forall eW: eWriteRequest, e.v != eW.v

payload SingleKey =
    on e: eWriteRequest with
    forall eW: eWriteRequest, e.k == eW.k

para cs: set[constraint];
para numWriteReq: int;
para numReadReq: int;
para numShutDown: int;
para nodes: set[machine];

syn Client {
    gen = [(eWriteRequest, numWriteReq), (eReadRequest, numReadReq), (eShutDown, numShutDown)];
    constriants = cs;
}

test param (chainSize in [3, 4], 
            numWriteReq in [2, 3],
            numReadReq in [2, 3],
            numShutDown in [1],
            cs in [[!strongConsistency, writeHead, readTail, crashHead], [!strongConsistency, writeHead, readTail, crashTail]]) 
     test1 [main=...]:
     assert ... in (union Client, ...);
