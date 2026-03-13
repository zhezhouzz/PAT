from common import *
import argparse

bench_json = []

benchmarks = ["graph", "nfa", "stlc", "stack", "filesystem", "ifc_store", "ifc_add", "ifc_load"]

spec_file = {
    "graph": "ADT/graph_spec.ml",
    "nfa": "ADT/nfa_spec.ml",
    "stlc": "ADT/stlc_spec.ml",
    "stack": "ADT/stack_spec.ml",
    "filesystem": "ADT/filesystem_spec.ml",
    "ifc_store": "ADT/ifc_spec.ml",
    "ifc_add": "ADT/ifc_spec.ml",
    "ifc_load": "ADT/ifc_spec.ml",
    "cart_rc": "MonkeyDB/cart_rc_spec.ml",
    "cart_cc": "MonkeyDB/cart_cc_spec.ml",
    "twitter_rc": "MonkeyDB/twitter_rc_spec.ml",
    "twitter_cc": "MonkeyDB/twitter_cc_spec.ml",
    "smallbank_rc": "MonkeyDB/smallbank_rc_spec.ml",
    "smallbank_cc": "MonkeyDB/smallbank_cc_spec.ml",
    "twitter_rc": "MonkeyDB/twitter_rc_spec.ml",
    "twitter_cc": "MonkeyDB/twitter_cc_spec.ml",
    "smallbank_rc": "MonkeyDB/smallbank_rc_spec.ml",
    "smallbank_cc": "MonkeyDB/smallbank_cc_spec.ml",
}

def run_syn(name):
    cmd = cmd_prefix + ["do-syn", name, "benchmarks/" + spec_file[name]]
    invoc_cmd(cmd)

def run_eval(name, testNum):
    cmd = cmd_prefix + ["test-eval", name, str(testNum)]
    invoc_cmd(cmd)

def run_random(name, testNum):
    cmd = cmd_prefix + ["test-random", name, str(testNum)]
    invoc_cmd(cmd)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Run 2025 benchmarks')
    parser.add_argument('name', type=str, help='Benchmark name')
    parser.add_argument('testNum', type=int, help='Test number')
    args = parser.parse_args()

    name = args.name
    testNum = args.testNum
    build_and_copy_exe()
    print("\n\n\n\nRunning synthesis for", name)
    run_syn(name)
    input()
    print("Running evaluation for", name, testNum)
    run_eval(name, testNum)
