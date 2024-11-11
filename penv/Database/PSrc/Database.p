machine Database {
    var is_init: bool;
    var store: int;
    var cache: seq[tsyn_writeRsp]; 
    start state Init {
        entry {
            is_init = false;
            store = -1;
            goto Wait;
        }
        
    }
    state Wait {
        entry{
            var input: tsyn_writeRsp;
            if(sizeof(cache) >= 2) {
                do_send_writeRsp(cache[1]);
                do_send_writeRsp(cache[0]);
            }
        }
        on syn_writeReq do (input: tsyn_writeReq) {
            is_init = true;
            store = input.va;
            cache += (sizeof(cache), (controller = input.controller, dst = input.controller, va = input.va));
            goto Wait;
        }
        on syn_readReq do (input: tsyn_readReq) {
            if(is_init) {
                send input.controller, syn_readRsp, (controller = input.controller, dst = input.controller, va = store, st = true);
            } else {
                send input.controller, syn_readRsp, (controller = input.controller, dst = input.controller, va = store, st = false);
            }
            goto Wait;
        }
    }
    fun do_send_writeRsp(input: tsyn_writeRsp) {
        send input.controller, syn_writeRsp, input; 
    }
}