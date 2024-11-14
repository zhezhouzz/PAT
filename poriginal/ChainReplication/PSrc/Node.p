machine Head {
    var next: machine;
    start state Init {
        entry (_next: machine){
            next = _next;
        }
        on syn_writeReq do (input: tsyn_writeReq) {
            RealSend(next, syn_writeToMid, (controller = input.controller, dst = next, key = input.key, va = input.va, node = Node1));
        }
    }
}

machine Mid {
    var tail: machine;
    var store: map[tNode, map[tKey, int]];
    var cache: seq[tsyn_writeToTail];
    start state Init {
        entry (_tail: machine) {
            tail = _tail;
            store[Node1] = default(map[tKey, int]);
            store[Node2] = default(map[tKey, int]);
            goto Loop;
        }
    }
    state Loop {
        entry {
            var i: int;
            if (sizeof(cache) >= 2) {
                i = sizeof(cache) - 1;
                while(i >= 0) {
                    do_send_write_to_tail(cache[i]);
                    i = i - 1;
                }
                cache = default(seq[tsyn_writeToTail]);
            }
        }
        on syn_writeToMid do (input: tsyn_writeToMid) {
            store[input.node][input.key] = input.va;
            if (input.node == Node1) {
                RealSend(this, syn_writeToMid, (controller = input.controller, dst = this, key = input.key, va = input.va, node = Node2));
            } else {
                cache += (sizeof(cache), (controller = input.controller, dst = tail, key = input.key, va = input.va));
            }
            goto Loop;
        }
        on syn_readReq do (input: tsyn_readReq) {
            if (input.key in store[Node2]) {
                RealSend(input.controller, syn_readRsp, (controller = input.controller, dst = input.controller, key = input.key, va = store[Node2][input.key], st = true));
            } else {
                RealSend(input.controller, syn_readRsp, (controller = input.controller, dst = input.controller, key = input.key, va = -1, st = false));
            }
            goto Loop;
        }
    } 
    fun do_send_write_to_tail(input: tsyn_writeToTail) {
        RealSend(input.dst, syn_writeToTail, input);
    }
}

machine Tail {
    var store: map[tKey, int];
    var is_crashed: bool;
    var mid: machine;
    var cache: seq[tsyn_writeRsp];
    start state Init {
        entry {
            is_crashed = false;
            receive {
                case mkChain: (input: (mid: machine)) {
                    mid = input.mid;
                }
            }
            goto Loop;
        } 
    }
    state Loop {
        entry {
            var i: int;
            if (sizeof(cache) >= 2) {
                i = sizeof(cache) - 1;
                while(i >= 0) {
                    do_send_write_rsp(cache[i]);
                    i = i - 1;
                }
                cache = default(seq[tsyn_writeRsp]);
            }
        }
        on syn_writeToTail do (input: tsyn_writeToTail) {
            if (!is_crashed) {
                store[input.key] = input.va;
                cache += (sizeof(cache), (controller = input.controller, dst = input.controller, key = input.key, va = input.va)); 
            }
            goto Loop;
        }
        on syn_crashTail do (input: tsyn_crashTail) {
            is_crashed = true;
            goto Loop;
        }
        on syn_readReq do (input: tsyn_readReq) {
            if (!is_crashed) {
                if (input.key in store) {
                    RealSend(input.controller, syn_readRsp, (controller = input.controller, dst = input.controller, key = input.key, va = store[input.key], st = true));
                } else {
                    RealSend(input.controller, syn_readRsp, (controller = input.controller, dst = input.controller, key = input.key, va = -1, st = false));
                }
            } else {
                RealSend(mid, syn_readReq, input); 
            }
            goto Loop;
        }
    }
    fun do_send_write_rsp(input: tsyn_writeRsp) {
        RealSend(input.dst, syn_writeRsp, input);
    } 
}
