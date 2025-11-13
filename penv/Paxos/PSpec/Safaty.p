event an_syn_ePrepareReq: tsyn_ePrepareReq;
event an_syn_ePrepareRsp: tsyn_ePrepareRsp;
event an_syn_eLostPrepareReq: tsyn_eLostPrepareReq;
event an_syn_eLostPrepareRsp: tsyn_eLostPrepareRsp;
event an_syn_eAcceptReq: tsyn_eAcceptReq;
event an_syn_eAcceptRsp: tsyn_eAcceptRsp;
event an_syn_eLostAcceptReq: tsyn_eLostAcceptReq;
event an_syn_eLostAcceptRsp: tsyn_eLostAcceptRsp;
event an_syn_eLearn: tsyn_eLearn;

spec leanerConsistentView observes an_syn_eLearn {
  var store: int;
  var is_init: bool;
  start state Init {
    entry{
      is_init = false;
    }
    on an_syn_eLearn do (input: tsyn_eLearn) {
      if (is_init) {
        assert (input.va == store), "property violation";  
      } else {
        is_init = true;
        store = input.va;
      }
    }
  }
}