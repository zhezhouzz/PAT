import subprocess
import json
import os
import sys
import time

bench_json = []

verbose = False

cmd_prefix = ["dune", "exec", "--", "bin/main.exe"]

def invoc_cmd(cmd, cwd=None):
    if (verbose):
        print(" ".join(cmd))
    try:
        subprocess.run(cmd, cwd=cwd)
    except subprocess.CalledProcessError as e:
        print(e.output)

benchmarks = ["graph", "nfa", "stlc", "stack", "filesystem", "ifc_store", "ifc_add", "ifc_load"]

spec_file = {
    "graph": "graph_spec.ml",
    "nfa": "nfa_spec.ml",
    "stlc": "stlc_spec.ml",
    "stack": "stack_spec.ml",
    "filesystem": "filesystem_spec.ml",
    "ifc_store": "ifc_spec.ml",
    "ifc_add": "ifc_spec.ml",
    "ifc_load": "ifc_spec.ml"
}

def run_syn(name):
    cmd = cmd_prefix + ["do-syn", name, "benchmarks/ADT/" + spec_file[name]]
    invoc_cmd(cmd)

def run_eval(name, testNum):
    cmd = cmd_prefix + ["test-eval", name, str(testNum)]
    invoc_cmd(cmd)

def run_random(name, testNum):
    cmd = cmd_prefix + ["test-random", name, str(testNum)]
    invoc_cmd(cmd)

if __name__ == "__main__":
    name = sys.argv[1]
    testNum = int(sys.argv[2])
    print("Running synthesis for", name)
    run_syn(name)
    input()
    print("Running evaluation for", name, testNum)
    run_eval(name, testNum)
    input()
    print("Running random for", name, testNum)
    input()
    run_random(name, testNum)
