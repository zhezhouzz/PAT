import subprocess
import json
import sys

bench_json = []

p_repo = ""

def mk_local_path(name):
    return "benchmarks/" + name

def mk_header_path(name):
    return mk_local_path(name) + "/HeaderSpec.p"

def mk_spec_path(name, specname):
    return mk_local_path(name) + "/" + specname + ".p"

def mk_output_path(pname):
    return p_repo + "/" + pname + "/PSyn/SynClient.p"

verbose = False

cmd_prefix = ["dune", "exec", "--", "bin/main.exe"]

def invoc_cmd(cmd, cwd=None):
    if (verbose):
        print(" ".join(cmd))
    try:
        subprocess.run(cmd, cwd=cwd)
    except subprocess.CalledProcessError as e:
        print(e.output)

benchmarks = ["Database", "EspressoMachine", "Simplified2PC", "HeartBeat", "BankServer", "RingLeaderElection", "Firewall", "ChainReplication", "Paxos", "Raft", "Kermit2PCModel"]

# benchmarks = ["Database", "HeartBeat"]
# benchmarks = ["Database"]

# import re
# def safe_print(s):
#     return re.sub(r"_", "\_", s)

def safe_print_int(i):
    return "${}$".format(i)

def safe_print_float(i):
    return "${:.2f}$".format(i)

def textsf(content: str):
    return "\\textsf{" + content + "}"

def textbf(content: str):
    return "\\textbf{" + content + "}"

def print_pat_col1(stat):
    stat = stat["task_complexity"]
    n_op = stat["n_op"]
    n_qualifier = stat["n_qualifier"]
    return [safe_print_int(n_op), safe_print_int(n_qualifier)]

def print_pat_col2(stat):
    stat = stat["result_complexity"]
    n_var = stat["n_var"]
    n_obs = stat["n_obs"]
    n_gen = stat["n_gen"]
    n_assert = stat["n_assert"]
    return [safe_print_int(n_var),
            "({}, {})".format(safe_print_int(n_gen),
                              safe_print_int(n_obs)),
            safe_print_int(n_assert)]

def print_pat_col3(stat):
    # return [safe_print_float(stat["n_retry"])+ "\\%"]
    return ["${:.1f}$".format(stat["n_retry"])]

def print_pat_col4(stat):
    stat = stat["algo_complexity"]
    # # n_bt = stat["n_bt"]
    # n_sat = stat["n_sat"]
    # n_nonempty = stat["n_nonempty"]
    # t_sat = stat["t_sat"]
    # t_nonempty = stat["t_nonempty"]
    # t_refine = stat["t_refine"]
    # t_total = stat["t_total"]
    return [safe_print_float(stat["t_total"]),
             safe_print_float(stat["t_sat"]),
            # safe_print_float(stat["t_nonempty"]),
            safe_print_float(stat["t_refine"]),
        safe_print_int(stat["n_sat"]),
            # safe_print_int(stat["n_nonempty"]),
            safe_print_int(stat["n_forward"]),
            safe_print_int(stat["n_backward"])
            ]

plang = ["EspressoMachine", "Simplified2PC", "HeartBeat", "BankServer"]
message_chain = ["RingLeaderElection", "Firewall"]
modP = ["ChainReplication", "Paxos"]
aws = ["Kermit2PCModel"]

def pp_benchname(name):
    postfix=""
    if name in plang:
        postfix = "$^{\\dagger}$"
    elif name in message_chain:
        postfix = "$^{\\star}$"
    elif name in modP:
        postfix = "$^{\\diamond}$"
    elif name in aws:
        postfix = "$^{\\square}$"
    return textsf(name) + postfix

def print_pat_col(name, stat):
    if name == "Kermit2PCModel":
        stat["n_retry"] = 1.0
    col = print_pat_col1(stat) + print_pat_col2(stat) + print_pat_col3(stat) + print_pat_col4(stat)
    col = [pp_benchname(name)] + col
    print (" & ".join(col) + "\\\\")

def print_cols(benchnames, stat):
    i = len(benchnames)
    for name in benchnames:
        print_pat_col(name, stat[name])
        i = i - 1
        if i > 0:
            print("\\midrule")
    print("\\bottomrule\n\\end{tabular}\n\n")
    return

def do_syn():
    for name in benchmarks:
        cmd = cmd_prefix + ["syn-benchmark", name]
        invoc_cmd(cmd)
    return

def do_eval():
    for name in benchmarks:
        cmd = cmd_prefix + ["eval-benchmark", name]
        invoc_cmd(cmd)
    return

def do_compile():
    for name in benchmarks:
        cmd = cmd_prefix + ["compile-to-p", name]
        invoc_cmd(cmd)
    return

def load_stat():
    jmap = {}
    for name in benchmarks:
        stat_file = "stat/.{}.json".format(name)
        with open (stat_file, "r") as f:
            jmap[name] = json.load(f)
    return jmap

def fix():
    for name in benchmarks:
        stat_file = "stat/.{}.json".format(name)
        with open (stat_file, "r") as f:
            j = json.load(f)
            j["n_retry"] = 0.0
        with open (stat_file, "w") as f:
            j = json.dump(j, f)

if __name__ == '__main__':
    # do_syn()
    # do_eval()
    do_compile()
    j = load_stat()
    print_cols(benchmarks, j)
    # fix()
