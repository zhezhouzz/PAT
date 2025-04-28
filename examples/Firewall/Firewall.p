event eInternalReq: (src: int, dst: int, id: int);
event eForwardReq: (src: int, dst: int, id: int);
event eExternalReq: (src: int, dst: int, id: int);
event eExternalRsp: (src: int, dst: int, id: int);

pat eInternalReq (tInternal <: int, tExternal <: int, tId <: int) with (input: (src: tInternal, dst: tExternal, id: tId)) {
    history = true;
    current = true;
    future = exists e: eForwardReq. e.src == input.src && e.dst: input.dst && e.id == input.id;
}

pat eForwardReq (tInternal <: int, tExternal <: int, tId <: int) with (input: (src: tInternal, dst: tExternal, id: tId)) {
    history = true;
    current = true;
    future = exists e: eExternalReq. e.src == input.dst && e.dst == input.src && e.id == input.id;
}

pat eExternalReq (tInternal <: int, tExternal <: int, tId <: int) with (input: (src: tExternal, dst: tInternal, id: tId)) {
    history = exists e1: eForwardReq. e1.dst == input.src &&
                                        (forall e2: eForwardReq. after(e1, e2) ==> e.dst == input.src);
    current = true;
    future = exists e: eExternalRsp. e.src == input.src && e.dst == input.dst && e.id == input.id;
} && {
    history = forall e1: eForwardReq. e1.dst == input.src ==> (exists e2: eForwardReq. after(e1, e2) && e.dst != input.src);
    current = true;
    future = forall e: eExternalRsp. e.id != input.id
}

pat eExternalRsp;

prop unique_internal_id = forall e1 e2: eInternalReq. e1.id != e2.id;
prop not_allow_all_session_from_internal_node = exists e1: eInternalReq. forall e2: eExternalRsp. after(e1, e2) ==> e2.id != e1.id;

syn machine SynClient (input: (internalNodes: set[int], externalNodes: set[int], ids: set[int])) = {
    gen = [eInternalReq input.internalNodes input.externalNodes input.ids];
    obs = [eExternalRsp input.internalNodes input.externalNodes input.ids];
    hidden = [eForwardReq input.internalNodes input.externalNodes input.ids, eExternalReq input.internalNodes input.externalNodes input.ids];
    props = [unique_internal_id, not_allow_all_session_from_internal_node]
}