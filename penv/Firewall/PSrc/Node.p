machine Internal {
    var fw: machine;
    start state Init {
        entry {
            receive {
                case connectFW: (input: (fw: machine)) {
                    fw = input.fw;
                }
            }
        }
        on syn_eStart do (input: tsyn_eStart) {
            send input.controller, syn_eInternalReq, (controller = input.controller, dst = fw, node = input.node);
        }
        ignore syn_eExternalRsp;
    }
}

machine Firewall {
    var internal: machine;
    var external: machine;
    var lastNode: tNode;
    start state Init {
        entry (input: (internal: machine, external: machine)) {
            internal = input.internal;
            external = input.external;
            goto Loop;
        }
    }
    state Loop {
        entry {
            var i: int;
        }
        on syn_eInternalReq do (input: tsyn_eInternalReq) {
            lastNode = input.node;
            do_send_forward((controller = input.controller, dst = external, node = input.node));
            goto Loop;
        }

        on syn_eExternalReq do (input: tsyn_eExternalReq) {
            if (lastNode == input.node) {
                send input.controller, syn_eExternalRsp, (controller = input.controller, dst = internal, node = input.node, stat = true);
            } else {
                send input.controller, syn_eExternalRsp, (controller = input.controller, dst = internal, node = input.node, stat = false);   
            }
            goto Loop;
        }
    }
    fun do_send_forward (input: tsyn_eForwardReq) {
        send input.controller, syn_eForwardReq, input; 
    }
}

machine External {
    var fw: machine;
    start state Init {
        entry {
            receive {
                case connectFW: (input: (fw: machine)) {
                    fw = input.fw;
                }
            }
        }

        on syn_eForwardReq do (input: tsyn_eForwardReq) {
            send input.controller, syn_eExternalReq, (controller = input.controller, dst = fw, node = input.node);
        }
    }
}