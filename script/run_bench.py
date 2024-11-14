import subprocess
import json
import os
import sys
import time

bench_json = []

random_stat_file = "stat/.run_random_p.json"
syn_stat_file = "stat/.run_syn_p.json"
default_stat_file = "stat/.run_default_p.json"

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

benchmarks = ["Database", "Firewall", "RingLeaderElection", "EspressoMachine", "BankServer", "Simplified2PC", "HeartBeat", "ChainReplication", "Paxos", "Raft", "Anno2PCModel"]
# benchmarks = ["ChainReplication", "Paxos", "Raft"]
# benchmarks = ["Raft"]

def syn_num_map(name):
    return 500

def default_num_map(name):
    return 2000

dict = {"Database":10000,
        "EspressoMachine":10000,
        "Simplified2PC":10000,
        "HeartBeat":10000,
        "BankServer":10000,
        "RingLeaderElection":10000,
        "Firewall":50,
        "ChainReplication":10000,
        "Paxos":10000,
        "Raft":1000,
        "Anno2PCModel": 1000}

def random_num_map(name):
    return dict[name]

# import re
# def safe_print(s):
#     return re.sub(r"_", "\_", s)

def safe_print_int(i):
    return "${}$".format(i)

def raw_safe_print_time(i):
    if i is None:
        return "-"
    else:
        return "{:.2f}".format(i)

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

def print_tries(ratio):
    if ratio is None:
        return "-"
    elif ratio == 0.0:
        return "{\\tiny Timeout}"
    else:
        return "${:.0f}$".format(100.0 / ratio)

def print_pat_col3(stat):
    # return [safe_print_float(stat["n_retry"])+ "\\%"]
    # return ["${:.1f}$".format(stat["n_retry"])]
    # return ["$({:.0f}\\%, {:.2f}\\%)$".format(stat["syn_ratio"], stat["random_ratio"])]
    return [ print_tries(stat["syn_ratio"]), print_tries(stat["default_ratio"]), print_tries(stat["random_ratio"])]

def print_pat_col4(statA):
    stat = statA["algo_complexity"]
    return [
        # "$({},{})$".format(raw_safe_print_time(statA["syn_time"]), raw_safe_print_time(statA["random_time"])),
        # "${}$".format(raw_safe_print_time(statA["random_time"])),
        safe_print_float(stat["t_total"]),
             # safe_print_float(stat["t_sat"]),
            # safe_print_float(stat["t_nonempty"]),
            # safe_print_float(stat["t_refine"]),
        safe_print_int(stat["n_sat"]),
            # safe_print_int(stat["n_nonempty"]),
            safe_print_int(stat["n_forward"]),
            safe_print_int(stat["n_backward"])
            ]

plang = ["EspressoMachine", "Simplified2PC", "HeartBeat", "BankServer"]
message_chain = ["RingLeaderElection", "Firewall"]
modP = ["ChainReplication", "Paxos"]
aws = ["Anno2PCModel"]

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
    col = print_pat_col1(stat) + print_pat_col2(stat) + print_pat_col3(stat) + print_pat_col4(stat)
    col = [pp_benchname(name)] + col
    print (" & ".join(col) + "\\\\")

def load_stat():
    jmap = {}
    for name in benchmarks:
        stat_file = "stat/.{}.json".format(name)
        with open (stat_file, "r") as f:
            jmap[name] = json.load(f)
    return jmap

def load_eval_stat(filename):
    if not os.path.exists(filename):
        with open(filename, 'w') as f:
            f.write("")
    with open (filename, "r") as f:
        data = json.load(f)
    return data

def print_cols(benchnames, stat):
    random_stat = load_eval_stat(random_stat_file)
    syn_stat = load_eval_stat(syn_stat_file)
    default_stat = load_eval_stat(default_stat_file)
    for name in benchnames:
        if name == "Anno2PCModel":
            random_stat[name] = [0.0, None]
            syn_stat[name] = [100.0, 0.1]
            default_stat[name] = [None, None]
            stat[name]["n_retry"] = 1.0
        stat[name]["random_ratio"] = random_stat[name][0]
        stat[name]["random_time"] = random_stat[name][1]
        stat[name]["syn_ratio"] = syn_stat[name][0]
        stat[name]["syn_time"] = syn_stat[name][1]
        stat[name]["default_ratio"] = default_stat[name][0]
        stat[name]["default_time"] = default_stat[name][1]
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

def run_syn_p_one(postfix, num, mode, kw):
    cur_dir = os.getcwd()
    # print(cur_dir)
    new_dir = cur_dir + "/" + postfix
    os.chdir(new_dir)
    start_time = time.time()
    result = subprocess.run("../../script/run_p.sh {} {} {}".format(mode, str(num), kw), shell=True, stdout=subprocess.PIPE, text=True, check=True)
    end_time = time.time()
    elapsed_time = end_time - start_time
    success = result.stdout.split(' ')
    success = [ int(str) for str in success if str.isnumeric()][0]
    avg_time = None
    if success != 0:
        avg_time = elapsed_time / success
    print("{}/{} ~ {} ==> {}".format(success, num, elapsed_time, avg_time))
    ratio = float(success * 100) / num
    # print("Output:", success)
    os.chdir(cur_dir)
    return (ratio, avg_time)

def run_syn_p():
    data = load_eval_stat(syn_stat_file)
    for name in benchmarks:
        if name == "Anno2PCModel":
            continue
        kw = "PSpec"
        if name == "RingLeaderElection" or name == "Paxos":
            kw = ""
        (ratio, avg_time) = run_syn_p_one("penv/" + name, syn_num_map(name), "Syn", kw)
        data[name] = (ratio, avg_time)
    with open(syn_stat_file, 'w') as fp:
        json.dump(data, fp)
    return

def run_random_p():
    data = load_eval_stat(random_stat_file)
    for name in benchmarks:
        if name == "Anno2PCModel":
            continue
        kw = "PSpec"
        if name == "RingLeaderElection" or name == "Paxos":
            kw = ""
        (ratio, avg_time) = run_syn_p_one("poriginal/" + name, random_num_map(name), "Syn", kw)
        data[name] = (ratio, avg_time)
    with open(random_stat_file, 'w') as fp:
        json.dump(data, fp)
    return

def run_default_p():
    data = load_eval_stat(default_stat_file)
    for name in benchmarks:
        if name == "Anno2PCModel":
            continue
        kw = "PSpec"
        if name == "Database" or name == "Firewall" or name == "RingLeaderElection" or name == "Raft":
            data[name] = (None, None)
            continue
        (ratio, avg_time) = run_syn_p_one("poriginal/" + name, default_num_map(name), "Manual", kw)
        data[name] = (ratio, avg_time)
    with open(default_stat_file, 'w') as fp:
        json.dump(data, fp)
    return

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
    # do_compile()
    # run_syn_p()
    # run_random_p()
    # run_default_p()
    j = load_stat()
    print_cols(benchmarks, j)
    # fix()
