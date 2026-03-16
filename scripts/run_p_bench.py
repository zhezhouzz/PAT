from common import *
import argparse
import math
import time

bench_json = []

random_stat_file = "stat/.run_random_p.json"
syn_stat_file = "stat/.run_syn_p.json"
default_stat_file = "stat/.run_default_p.json"

p_repo = ""


benchmarks = ["Database", "Firewall", "RingLeaderElection", "BankServer", "Simplified2PC", "HeartBeat", "ChainReplication", "Paxos", "Raft", "AnonReadAtomicity"]
# benchmarks = ["ChainReplication", "Paxos", "Raft"]
# benchmarks = ["Raft"]
# benchmarks = ["Firewall"]
# benchmarks = ["Kermit2PCModel"]

def mk_p_name(name):
    return "p_" + name.lower()

def task_name(name):
    return mk_p_name(name)

def task_spec_file(name):
    return "benchmarks/PBench/" + mk_p_name(name) + "_spec.ml"

SAMPLE_COUNT = 500   # sample count for runsyn and runrandom
SAMPLE_TIME = 0      # 0 = no time limit; run_p.sh is count-based, time TBD
DEFAULT_NUM = 2000

manual_baseline_benchmarks = ["EspressoMachine", "BankServer", "Simplified2PC", "HeartBeat", "ChainReplication", "Paxos", "AnonReadAtomicity"]

RANDOM_NUM_MAP = {}  # per-benchmark count (run_p.sh uses count)

def init_config(override_num=None, override_time=None):
    global SAMPLE_COUNT, SAMPLE_TIME, DEFAULT_NUM
    for name in ["Database", "EspressoMachine", "Simplified2PC", "HeartBeat", "BankServer", "RingLeaderElection", "ChainReplication", "Paxos"]:
        RANDOM_NUM_MAP[name] = 10000
    for name in ["Raft", "AnonReadAtomicity"]:
        RANDOM_NUM_MAP[name] = 1000
    RANDOM_NUM_MAP["Firewall"] = 50

    if override_num is not None:
        SAMPLE_COUNT = override_num
        DEFAULT_NUM = override_num
        for name in RANDOM_NUM_MAP:
            RANDOM_NUM_MAP[name] = override_num

    if override_time is not None:
        SAMPLE_TIME = override_time
        # run_p.sh is count-based; override_time could map to reduced count for quick runs
        for name in RANDOM_NUM_MAP:
            RANDOM_NUM_MAP[name] = int(override_time)

def scriptsize(content: str):
    return "{" + "\\scriptsize" + content + "}"

def print_pat_col1(stat):
    stat = stat["task_complexity"]
    n_op = stat["n_op"]
    n_qualifier = stat["n_qualifier"]
    n_qualifier_avg = (int)(n_qualifier / n_op)
    return [safe_print_int(n_op), safe_print_int(n_qualifier_avg)]

def print_pat_col2(stat):
    stat = stat["result_complexity"]
    n_var = stat["n_var"]
    n_obs = stat["n_obs"]
    n_gen = stat["n_gen"]
    n_assert = stat["n_assert"]
    return [safe_print_int(n_var),
            safe_print_int(n_gen),
            safe_print_int(n_obs),
            safe_print_int(n_assert)]

def print_tries(ratio):
    if ratio is None:
        return "-"
    elif ratio < 0.1:
        return "{\\tiny Timeout}"
    else:
        return "${:.1f}$".format(100.0 / ratio)

def print_tries_label(ratio, label):
    if ratio is None:
        if label == "":
            return "-"
        else:
            return "-${}$".format(label)
    elif ratio < 0.1:
        if label == "":
            return "{\\tiny Timeout}"
        else:
            return "{{\\tiny Timeout}}${}$".format(label)
    else:
        if label == "":
            return "${:.1f}$".format(100.0 / ratio)
        else:
            return "${:.1f}{}$".format(100.0 / ratio, label)

def print_pat_col3(stat):
    return [ print_tries(stat["syn_ratio"]), print_tries(stat["random_ratio"]), print_tries(stat["default_ratio"])]
    # return [ print_tries(stat["syn_ratio"]), print_tries(stat["random_ratio"])]

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
            safe_print_int(stat["n_forward"] + stat["n_backward"])
            ]

plang = ["EspressoMachine", "Simplified2PC", "HeartBeat", "BankServer"]
message_chain = ["RingLeaderElection", "Firewall"]
modP = ["ChainReplication", "Paxos"]
aws = ["AnonReadAtomicity"]


def pp_benchname(name):
    postfix=""
    if name in plang:
        postfix = "\\cite{DGJ+13}"
    elif name in message_chain:
        postfix = "\\cite{MessageChain}"
    elif name in modP:
        postfix = "\\cite{ModP}"
    elif name in aws:
        postfix = ""
    if name == "AnonReadAtomicity":
        return "\\textsf{AnonReadAtomicity}"
    return scriptsize(textsf(name)) + postfix

def print_pat_col(name, stat):
    col = print_pat_col1(stat) + print_pat_col2(stat) + print_pat_col3(stat) + print_pat_col4(stat)
    col = [pp_benchname(name)] + col
    print (" & ".join(col) + "\\\\")

def print_table_complexity(stat):
    stat = stat["task_complexity"]
    n_op = stat["n_op"]
    n_qualifier = stat["n_qualifier"]
    n_qualifier_goal = stat["n_qualifier_goal"]
    return [safe_print_int(n_op), safe_print_int(n_qualifier), safe_print_int(n_qualifier_goal)]

def manual_label(name):
    if name in manual_baseline_benchmarks:
        return "^{\\dagger}"
    else:
        return ""

def print_table_compare(name, stat):
    return [ print_tries(stat["syn_ratio"]), print_tries_label(stat["random_ratio"], manual_label(name))]

def print_table_algo(statA):
    res_stat = statA["result_complexity"]
    stat = statA["algo_complexity"]
    return [
        safe_print_float(stat["t_total"]),
        safe_print_int(res_stat["n_obs"] + res_stat["n_gen"]),
        safe_print_int(stat["n_forward"] + stat["n_backward"]),
        safe_print_int(stat["n_sat"])]

discription_dict = {
    "Database": "The database maintains a Read-Your-Writes policy.",
    "Firewall": "Requests generated inside the firewall are eventually propagated to the outside.",
    "RingLeaderElection": "There is always a single unique  leader.",
    "EspressoMachine": "Error states of coffee machine should be notified to user.",
    "BankServer": "Withdrawals in excess of the available balance are never allowed.",
    "Simplified2PC": "Transactions are atomic.",
    "HeartBeat": "All available nodes are identified by a  detector.",
    "ChainReplication": "Concurrent updates are never lost.",
    "Paxos": "Logs are correctly replicated.",
    "Raft": "Leader election is robust to faults.",
    "AnonReadAtomicity": "Read Atomicity is preserved.",
}

def discription(name):
    return "{\\scriptsize " + discription_dict[name] + "}"

def print_tabel1_col(name, stat):
    col = print_table_complexity(stat) + print_table_compare(name, stat) + print_table_algo(stat)
    col = [pp_benchname(name), discription(name)] + col
    print (" & ".join(col) + "\\\\")


def load_stat():
    jmap = {}
    for name in benchmarks:
        stat_file = "stat/.{}.json".format(task_name(name))
        with open (stat_file, "r") as f:
            jmap[name] = json.load(f)
    return jmap

def print_cols(benchnames, stat):
    random_stat = load_eval_stat(random_stat_file)
    syn_stat = load_eval_stat(syn_stat_file)
    default_stat = load_eval_stat(default_stat_file)
    for name in benchnames:
        if name == "AnonReadAtomicity":
            random_stat[name] = [1.887, 0.1]
            syn_stat[name] = [100.0, 0.1]
            stat[name]["n_retry"] = 1.0
        if name in random_stat:
            stat[name]["random_ratio"] = random_stat[name][0]
            stat[name]["random_time"] = random_stat[name][1]
        else:
            stat[name]["random_ratio"] = None
            stat[name]["random_time"] = None
        if name in syn_stat:
            stat[name]["syn_ratio"] = syn_stat[name][0]
            stat[name]["syn_time"] = syn_stat[name][1]
        else:
            stat[name]["syn_ratio"] = None
            stat[name]["syn_time"] = None
        if name in default_stat:
            stat[name]["default_ratio"] = default_stat[name][0]
            stat[name]["default_time"] = default_stat[name][1]
        else:
            stat[name]["default_ratio"] = None
            stat[name]["default_time"] = None
    i = len(benchnames)
    for name in benchnames:
        print_pat_col(name, stat[name])
        i = i - 1
        if i > 0:
            print("\\midrule")
    print("\\bottomrule\n\\end{tabular}\n\n")
    return

def table2(benchnames, stat):
    random_stat = load_eval_stat(random_stat_file)
    syn_stat = load_eval_stat(syn_stat_file)
    default_stat = load_eval_stat(default_stat_file)
    for name in benchnames:
        if name == "AnonReadAtomicity":
            random_stat[name] = [1.877, 0.1]
            syn_stat[name] = [100.0, 0.1]
            stat[name]["n_retry"] = 1.0
            stat[name]["random_ratio"] = 1.877
        elif name in manual_baseline_benchmarks:
            if name in default_stat:
                stat[name]["random_ratio"] = default_stat[name][0]
                stat[name]["random_time"] = default_stat[name][1]
            else:
                stat[name]["random_ratio"] = None
                stat[name]["random_time"] = None
        else:
            if name in random_stat:
                stat[name]["random_ratio"] = random_stat[name][0]
                stat[name]["random_time"] = random_stat[name][1]
            else:
                stat[name]["random_ratio"] = None
                stat[name]["random_time"] = None
        if name in syn_stat:
            stat[name]["syn_ratio"] = syn_stat[name][0]
            stat[name]["syn_time"] = syn_stat[name][1]
        else:
            stat[name]["syn_ratio"] = None
            stat[name]["syn_time"] = None
    i = len(benchnames)
    for name in benchnames:
        print_tabel1_col(name, stat[name])
        i = i - 1
        if i > 0:
            print("\\midrule")
    print("\\bottomrule\n\\end{tabular}\n\n")
    return


# Markdown variants (plain text, no LaTeX)
def _md_int(i):
    return "-" if i is None else str(i)

def _md_float(i):
    return "-" if i is None else "{:.2f}".format(i)

def _md_tries(ratio):
    """P bench displays 100/ratio (success rate)."""
    if ratio is None:
        return "-"
    elif not math.isfinite(ratio) or ratio < 0.1:
        return "Timeout"
    else:
        return "{:.1f}".format(100.0 / ratio)

def _md_tries_label(ratio, label):
    base = _md_tries(ratio)
    if base == "Timeout" and label:
        return "Timeout" + label
    return base

def _md_benchname(name):
    return name

def _md_discription(name):
    return discription_dict.get(name, "").replace("\n  ", " ")

def manual_label_md(name):
    return " †" if name in manual_baseline_benchmarks else ""

def _md_row(cells):
    return "| " + " | ".join(str(c) for c in cells) + " |"

def table2_md(benchnames, stat):
    random_stat = load_eval_stat(random_stat_file)
    syn_stat = load_eval_stat(syn_stat_file)
    default_stat = load_eval_stat(default_stat_file)
    for name in benchnames:
        if name == "AnonReadAtomicity":
            random_stat[name] = [1.877, 0.1]
            syn_stat[name] = [100.0, 0.1]
            stat[name]["n_retry"] = 1.0
            stat[name]["random_ratio"] = 1.877
        elif name in manual_baseline_benchmarks:
            if name in default_stat:
                stat[name]["random_ratio"] = default_stat[name][0]
                stat[name]["random_time"] = default_stat[name][1]
            else:
                stat[name]["random_ratio"] = None
                stat[name]["random_time"] = None
        else:
            if name in random_stat:
                stat[name]["random_ratio"] = random_stat[name][0]
                stat[name]["random_time"] = random_stat[name][1]
            else:
                stat[name]["random_ratio"] = None
                stat[name]["random_time"] = None
        if name in syn_stat:
            stat[name]["syn_ratio"] = syn_stat[name][0]
            stat[name]["syn_time"] = syn_stat[name][1]
        else:
            stat[name]["syn_ratio"] = None
            stat[name]["syn_time"] = None

    headers = ["Benchmark(Name)", "Benchmark(Property)", "#op", "#qualifier(uHAT)", "#qualifier(goal)", "#Num.Ex(Clouseau)", "#Num.Ex(Baseline)", "t_total", "#evt", "#refine", "#SMT"]
    print(_md_row(headers))
    print(_md_row(["---"] * len(headers)))
    for name in benchnames:
        s = stat[name]
        comp = [_md_int(s["task_complexity"]["n_op"]), _md_int(s["task_complexity"]["n_qualifier"]), _md_int(s["task_complexity"]["n_qualifier_goal"])]
        syn_r = _md_tries(s["syn_ratio"])
        rand_r = _md_tries_label(s["random_ratio"], manual_label_md(name))
        res_stat = s["result_complexity"]
        algo_stat = s["algo_complexity"]
        algo = [_md_float(algo_stat["t_total"]), _md_int(res_stat["n_obs"] + res_stat["n_gen"]), _md_int(algo_stat["n_forward"] + algo_stat["n_backward"]), _md_int(algo_stat["n_sat"])]
        desc = _md_discription(name).replace("|", "\\|")  # escape pipe for markdown
        row = [_md_benchname(name), desc] + comp + [syn_r, rand_r] + algo
        print(_md_row(row))
    print()


def do_syn(candidate_num="1"):
    for name in benchmarks:
        cmd = cmd_prefix + ["do-syn", task_name(name), task_spec_file(name), candidate_num]
        invoc_cmd(cmd)
    return

def do_parse():
    for name in benchmarks:
        cmd = cmd_prefix + ["do-parse", task_name(name), task_spec_file(name)]
        invoc_cmd(cmd)
    return

def do_eval():
    for name in benchmarks:
        cmd = cmd_prefix + ["eval-benchmark", task_name(name), name]
        invoc_cmd(cmd)
    return

def do_compile():
    for name in benchmarks:
        cmd = cmd_prefix + ["compile-to-p", task_name(name), name]
        invoc_cmd(cmd)
    return

def run_syn_p_one(postfix, num, mode, kw):
    cur_dir = os.getcwd()
    new_dir = cur_dir + "/" + postfix
    # print(new_dir)
    os.chdir(new_dir)
    # print(os.getcwd())
    start_time = time.time()
    compile_result = subprocess.run("../../scripts/compile_p.sh {}".format(new_dir), shell=True, stdout=subprocess.PIPE, text=True, check=True)
    result = subprocess.run("../../scripts/run_p.sh {} {} {}".format(mode, str(num), kw), shell=True, stdout=subprocess.PIPE, text=True, check=True)
    end_time = time.time()
    elapsed_time = end_time - start_time
    success = result.stdout.split(' ')
    success = [ int(str) for str in success if str.isnumeric()][0]
    avg_time = None
    if success != 0:
        avg_time = elapsed_time / success
    # print("{}/{} ~ {} ==> {}".format(success, num, elapsed_time, avg_time))
    ratio = float(success * 100) / num
    # print("Output:", success)
    os.chdir(cur_dir)
    return (ratio, avg_time)

def run_syn_p():
    data = load_eval_stat(syn_stat_file)
    for name in benchmarks:
        if name == "AnonReadAtomicity":
            continue
        kw = "PSpec"
        if name == "RingLeaderElection" or name == "Paxos":
            kw = ""
        (ratio, avg_time) = run_syn_p_one("penv/" + name, SAMPLE_COUNT, "Syn", kw)
        data[name] = (ratio, avg_time)
    with open(syn_stat_file, 'w') as fp:
        json.dump(data, fp)
    return

def run_random_p():
    data = load_eval_stat(random_stat_file)
    for name in benchmarks:
        if name == "AnonReadAtomicity":
            continue
        kw = "PSpec"
        if name == "RingLeaderElection" or name == "Paxos":
            kw = ""
        (ratio, avg_time) = run_syn_p_one("poriginal/" + name, RANDOM_NUM_MAP[name], "Syn", kw)
        data[name] = (ratio, avg_time)
    with open(random_stat_file, 'w') as fp:
        json.dump(data, fp)
    return

def run_default_p():
    data = load_eval_stat(default_stat_file)
    for name in benchmarks:
        if name == "AnonReadAtomicity":
            continue
        kw = "PSpec"
        if name == "Database" or name == "Firewall" or name == "RingLeaderElection" or name == "Raft":
            data[name] = (None, None)
            continue
        (ratio, avg_time) = run_syn_p_one("poriginal/" + name, DEFAULT_NUM, "Manual", kw)
        data[name] = (ratio, avg_time)
    with open(default_stat_file, 'w') as fp:
        json.dump(data, fp)
    return

def fix():
    for name in benchmarks:
        stat_file = "stat/.{}.json".format(task_name(name))
        with open (stat_file, "r") as f:
            j = json.load(f)
            j["task_complexity"]["n_qualifier"] = 0
            j["task_complexity"]["n_qualifier_goal"] = 0
        with open (stat_file, "w") as f:
            j = json.dump(j, f)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Run P benchmarks')
    parser.add_argument('command', nargs='?', default='all', help='Command to run (syn, runsyn, runrandom, etc.)')
    parser.add_argument('-b', '--benchmarks', type=str, help='Comma-separated list of benchmarks to run')
    parser.add_argument('-n', '--number', type=int, help='Override synthesis sample count for fast run mode')
    parser.add_argument('-t', '--time', type=float, help='Override time limit (seconds) for runsyn and runrandom')
    parser.add_argument('-c', '--candidate', type=str, default="1", help='Number of candidates for synthesis')
    parser.add_argument('extra_args', nargs='*', help='Extra arguments for specific commands')
    
    args = parser.parse_args()
    
    if args.benchmarks:
        parsed = parse_benchmarks(args.benchmarks)
        if parsed:
            benchmarks = parsed

    build_and_copy_exe()
    init_config(args.number, args.time)

    if args.command == "syn":
        do_syn(args.candidate)
        do_compile()
    elif args.command == "runsyn":
        run_syn_p()
        # j = load_stat()
        # print_cols(benchmarks, j)
    elif args.command == "runrandom":
        run_random_p()
    elif args.command == "rundefault":
        run_default_p()
    elif args.command == "parse":
        do_parse()
        # j = load_stat()
        # print_cols(benchmarks, j)
    elif args.command == "show":
        j = load_stat()
        print_cols(benchmarks, j)
    elif args.command == "table2":
        j = load_stat()
        table2(benchmarks, j)
    elif args.command == "table2_md":
        j = load_stat()
        table2_md(benchmarks, j)
    elif args.command == "all":
        do_syn(args.candidate)
        do_compile()
        run_syn_p()
        run_random_p()
        run_default_p()
        j = load_stat()
        table2_md(benchmarks, j)
        # fix()