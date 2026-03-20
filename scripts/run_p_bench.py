import common
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

def task_spec_file(name, use_simplified=False):
    suffix = "_simp_spec.ml" if use_simplified else "_spec.ml"
    return "benchmarks/PBench/" + mk_p_name(name) + suffix

SAMPLE_COUNT = 1000   # sample count for runsyn and runrandom
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
    RANDOM_NUM_MAP["Firewall"] = 1000

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
        return "{\\tiny TO}"
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
            return "{\\tiny TO}"
        else:
            return "{{\\tiny TO}}${}$".format(label)
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
        return "TO"
    else:
        return "{:.1f}".format(100.0 / ratio)

def _md_tries_label(ratio, label):
    base = _md_tries(ratio)
    if base == "TO" and label:
        return "TO" + label
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


def do_syn(candidate_num="1", use_simplified=False):
    for name in benchmarks:
        spec = task_spec_file(name, use_simplified)
        print(f"Synthesizing test generators for {name} ({spec})...\n")
        cmd = cmd_prefix + ["do-syn", task_name(name), spec, candidate_num]
        invoc_cmd(cmd)
    return

def do_parse(use_simplified=False):
    for name in benchmarks:
        spec = task_spec_file(name, use_simplified)
        cmd = cmd_prefix + ["do-parse", task_name(name), spec]
        invoc_cmd(cmd)
    return

def do_eval():
    for name in benchmarks:
        cmd = cmd_prefix + ["eval-benchmark", task_name(name), name]
        invoc_cmd(cmd)
    return

def do_compile():
    for name in benchmarks:
        print(f"Compiling synthesized test generators to P language for {name}...\n")
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
    if success == 0:
        print(f"Average tries to detect bugs (# Num. Executions): inf (tried {num} times, but no bug detected)")
    else:
        print(f"Average tries to detect bugs (# Num. Executions): {num/success:.2f}")
    os.chdir(cur_dir)
    return (ratio, avg_time)

def run_syn_p():
    data = load_eval_stat(syn_stat_file)
    for name in benchmarks:
        if name == "AnonReadAtomicity":
            print("Skipping AnonReadAtomicity as it is an internal benchmark from a commercial company.\n")
            continue
        kw = "PSpec"
        if name == "RingLeaderElection" or name == "Paxos":
            kw = ""
        print(f"Running synthesized test generators for {name}...\n")
        (ratio, avg_time) = run_syn_p_one("penv/" + name, SAMPLE_COUNT, "Syn", kw)
        data[name] = (ratio, avg_time)
    with open(syn_stat_file, 'w') as fp:
        json.dump(data, fp)
    return

def run_random_p():
    data = load_eval_stat(random_stat_file)
    for name in benchmarks:
        if name == "AnonReadAtomicity":
            print("Skipping AnonReadAtomicity as it is an internal benchmark from a commercial company.\n")
            continue
        kw = "PSpec"
        if name == "RingLeaderElection" or name == "Paxos":
            kw = ""
        print(f"Running random test generators for {name}...\n")
        (ratio, avg_time) = run_syn_p_one("poriginal/" + name, RANDOM_NUM_MAP[name], "Syn", kw)
        data[name] = (ratio, avg_time)
    with open(random_stat_file, 'w') as fp:
        json.dump(data, fp)
    return

def run_default_p():
    data = load_eval_stat(default_stat_file)
    for name in benchmarks:
        if name == "AnonReadAtomicity":
            print("Skipping AnonReadAtomicity as it is an internal benchmark from a commercial company.\n")
            continue
        kw = "PSpec"
        if name == "Database" or name == "Firewall" or name == "RingLeaderElection" or name == "Raft":
            data[name] = (None, None)
            continue
        print(f"Running manual test generators for {name}...\n")
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


TABLE2_MD_SIMP_JSON = "stat/table2_md_simp.json"


def save_table2_md_simp_json(benchnames, stat, filepath=None):
    """Save the numbers printed by table2_md to a JSON file."""
    if filepath is None:
        filepath = TABLE2_MD_SIMP_JSON
    data = {"headers": ["Benchmark(Name)", "Benchmark(Property)", "#op", "#qualifier(uHAT)", "#qualifier(goal)",
                        "#Num.Ex(Clouseau)", "#Num.Ex(Baseline)", "t_total", "#evt", "#refine", "#SMT"],
            "rows": []}
    for name in benchnames:
        s = stat[name]
        tc = s["task_complexity"]
        rc = s["result_complexity"]
        ac = s["algo_complexity"]
        row = {
            "name": name,
            "description": _md_discription(name),
            "n_op": tc["n_op"],
            "n_qualifier": tc["n_qualifier"],
            "n_qualifier_goal": tc["n_qualifier_goal"],
            "syn_ratio": s.get("syn_ratio"),
            "random_ratio": s.get("random_ratio"),
            "t_total": ac["t_total"],
            "n_evt": (rc["n_obs"] + rc["n_gen"]) if rc else None,
            "n_refine": (ac["n_forward"] + ac["n_backward"]) if ac else None,
            "n_sat": ac["n_sat"] if ac else None,
        }
        data["rows"].append(row)
    d = os.path.dirname(filepath)
    if d:
        os.makedirs(d, exist_ok=True)
    with open(filepath, "w") as f:
        json.dump(data, f, indent=2)
    print(f"Saved table2_md_simp data to {filepath}")
    return filepath


def load_table2_md_simp_json(filepath=None):
    """Load the table2_md_simp numbers from a JSON file. Returns None if file missing or invalid."""
    if filepath is None:
        filepath = TABLE2_MD_SIMP_JSON
    try:
        with open(filepath, "r") as f:
            return json.load(f)
    except Exception:
        return None


def _populate_table2_ratios(benchnames, stat):
    """Fill random_ratio and syn_ratio in stat from eval files (P bench logic)."""
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


def print_table_complexity_rich(stat, simp_row):
    """Like print_table_complexity but n_qualifier shows N/M (table2 / simp)."""
    tc = stat["task_complexity"]
    n_op = safe_print_int(tc["n_op"])
    n_qualifier_n = _md_int(tc["n_qualifier"])
    n_qualifier_m = _md_int(simp_row["n_qualifier"]) if simp_row else "-"
    n_qualifier = f"{n_qualifier_n}/{n_qualifier_m}"
    n_qualifier_goal = safe_print_int(tc["n_qualifier_goal"])
    return [n_op, n_qualifier, n_qualifier_goal]


def print_table_compare_rich(name, stat, simp_row):
    """Like print_table_compare but syn_ratio shows N/M (table2 / simp). Baseline unchanged."""
    syn_n = print_tries(stat["syn_ratio"])
    syn_m = print_tries(simp_row["syn_ratio"]) if simp_row and simp_row.get("syn_ratio") is not None else "-"
    rand_n = print_tries_label(stat["random_ratio"], manual_label(name))
    return [f"{syn_n}/{syn_m}", rand_n]


def print_tabel2_col_rich(name, stat, simp_row):
    """Like print_tabel1_col but uses _rich variants for n_qualifier and syn_ratio (baseline unchanged)."""
    comp = print_table_complexity_rich(stat, simp_row)
    compare = print_table_compare_rich(name, stat, simp_row)
    algo = print_table_algo(stat)
    col = [pp_benchname(name), discription(name)] + comp + compare + algo
    print(" & ".join(col) + "\\\\")


def table2_rich(benchnames, stat):
    """Like table2 but also shows simp spec values (N/M) for n_qualifier and syn_ratio. Baseline unchanged."""
    simp_data = load_table2_md_simp_json()
    simp_lookup = {r["name"]: r for r in simp_data.get("rows", [])} if simp_data else {}
    _populate_table2_ratios(benchnames, stat)

    i = len(benchnames)
    for name in benchnames:
        simp_row = simp_lookup.get(name)
        print_tabel2_col_rich(name, stat[name], simp_row)
        i = i - 1
        if i > 0:
            print("\\midrule")
    print("\\bottomrule\n\\end{tabular}\n\n")


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Run P benchmarks')
    parser.add_argument('command', nargs='?', default='all', help='Command to run (syn, runsyn, runrandom, etc.)')
    parser.add_argument('-b', '--benchmarks', type=str, help='Comma-separated list of benchmarks to run')
    parser.add_argument('-n', '--number', type=int, help='Override synthesis sample count for fast run mode')
    parser.add_argument('-t', '--time', type=float, help='Override time limit (seconds) for runsyn and runrandom')
    parser.add_argument('-c', '--candidate', type=str, default="1", help='Number of candidates for synthesis')
    parser.add_argument('-s', '--simplified', action='store_true', help='Use simplified spec files (*_simp_spec.ml)')
    parser.add_argument('-v', '--verbose', action='store_true', default=False, help='Enable verbose output (print commands)')
    parser.add_argument('extra_args', nargs='*', help='Extra arguments for specific commands')
    
    args = parser.parse_args()
    
    if args.benchmarks:
        parsed = parse_benchmarks(args.benchmarks)
        if parsed:
            benchmarks = parsed

    build_and_copy_exe()
    init_config(args.number, args.time)
    common.verbose = args.verbose

    if args.command == "syn":
        do_syn(args.candidate, use_simplified=args.simplified)
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
        do_parse(use_simplified=args.simplified)
        # j = load_stat()
        # print_cols(benchmarks, j)
    elif args.command == "show":
        j = load_stat()
        print_cols(benchmarks, j)
    elif args.command == "table2":
        j = load_stat()
        table2(benchmarks, j)
    elif args.command == "table2_rich":
        j = load_stat()
        table2_rich(benchmarks, j)
    elif args.command == "table2_md":
        j = load_stat()
        table2_md(benchmarks, j)
        save_table2_md_simp_json(benchmarks, j)
    elif args.command == "table2_md_simp":
        do_syn(args.candidate, use_simplified=args.simplified)
        do_compile()
        run_syn_p()
        j = load_stat()
        table2_md(benchmarks, j)
        save_table2_md_simp_json(benchmarks, j)
    elif args.command == "all":
        do_syn(args.candidate, use_simplified=args.simplified)
        do_compile()
        run_syn_p()
        run_random_p()
        run_default_p()
        j = load_stat()
        table2_md(benchmarks, j)
        # fix()