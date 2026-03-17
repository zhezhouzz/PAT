import common
from common import *
import argparse
import re
import sys

bench_json = []

random_stat_file = "stat/.run_random_p.json"
syn_stat_file = "stat/.run_syn.json"
default_stat_file = "stat/.run_default.json"

p_repo = ""


benchmarks = ["Stack", "Set", "Filesystem", "Graph", "NFA", "IFCStore", "IFCAdd", "IFCLoad", "DeBruijn1", "DeBruijn2", "Shopping", "HashTable", "Transaction", "Courseware", "Twitter", "Smallbank"]

discription_dict = {
    "Set": "Membership holds for every element inserted into the set.",
    "Stack": "Pushes and pops are correctly paired.",
    "HashTable": "No updates to a concurrent hashtable are every lost.",
    "Filesystem": "A valid file path only contains non-deleted entries.",
    "Graph": "A serialized stream of nodes and edges is re-constituted to\n  form a fully-connected graph.",
    "NFA": "An NFA reaches a final state for every string in the language it accepts.",
    "IFCAdd": "A well-behaved IFC program containing an $\\Code{Add}$ command never leaks a secret.",
    "IFCStore": "A well-behaved IFC program containing a $\\Code{Store}$ command never leaks a secret.",
    "IFCLoad": "A well-behaved IFC program containing a $\\Code{Load}$ command never leaks a secret.",
    "DeBruijn1": "An STLC interpreter correctly evaluates a\n  well-typed first-order STLC program that uses a de Brujin representation.",
    "DeBruijn2": "An STLC interpreter correctly evaluates a\n  a well-typed higher-order STLC program that uses a de Brujin representation.",
    "Transaction": "Asynchronous read operations are logically atomic.",
    "Shopping": "All items added to a cart can be checked-out.",
    "Courseware": "Every student enrolled in a course exists in the\n  enrollment database for that course.",
    "Twitter": "Posted tweets are visible to all followers.",
    "Smallbank": "Account updates are strongly consistent.",
    "TreiberStack": "Pushed value can be popped.",
}

def discription(name):
    return "{\\scriptsize " + discription_dict[name] + "}"

task_name_dict = {
    "Set": "set",
    "Stack": "stack",
    "Graph": "graph",
    "Filesystem": "filesystem",
    "NFA": "nfa",
    "IFCStore": "ifc_store",
    "IFCAdd": "ifc_add",
    "IFCLoad": "ifc_load",
    "DeBruijn1": "debruijn1",
    "DeBruijn2": "debruijn2",
    "HashTable": "hashtable",
    "Transaction": "transaction",
    "Shopping": "shopping",
    "Courseware": "courseware",
    "Twitter": "twitter",
    "Smallbank": "smallbank",
}

task_dir_dict = {
    "Set": "OCamlBench/set_spec.ml",
    "Stack": "OCamlBench/stack_spec.ml",
    "Graph": "OCamlBench/graph_spec.ml",
    "Filesystem": "OCamlBench/filesystem_spec.ml",
    "NFA": "OCamlBench/nfa_spec.ml",
    "IFCStore": "OCamlBench/ifc_spec.ml",
    "IFCAdd": "OCamlBench/ifc_spec.ml",
    "IFCLoad": "OCamlBench/ifc_spec.ml",
    "DeBruijn1": "OCamlBench/debruijn1_spec.ml",
    "DeBruijn2": "OCamlBench/debruijn2_spec.ml",
    "HashTable": "OCamlBench/hashtable_spec.ml",
    "Transaction": "MonkeyDB/transaction_spec.ml",
    "Shopping": "MonkeyDB/shopping_spec.ml",
    "Courseware": "MonkeyDB/courseware_spec.ml",
    "Twitter": "MonkeyDB/twitter_spec.ml",
    "Smallbank": "MonkeyDB/smallbank_spec.ml",
}

def task_name(name):
    return task_name_dict[name]

def task_dir(name):
    return "benchmarks/" + task_dir_dict[name].replace("ADT/", "")

SAMPLE_COUNT = "200"  # sample count for runsyn and runrandom
SAMPLE_TIME = "0"     # 0 = no time limit; used by runsyn; runrandom uses RANDOM_TIME_MAP
RANDOM_TIME_MAP = {}

def init_config(override_num=None, override_time=None):
    global SAMPLE_COUNT, SAMPLE_TIME
    if override_num is not None:
        SAMPLE_COUNT = str(override_num)

    for name in task_name_dict:
        if name in ["Set", "Stack", "Transaction", "Graph", "NFA"] or name in monkeydb:
            RANDOM_TIME_MAP[name] = "120"
        else:
            RANDOM_TIME_MAP[name] = "600"

    if override_time is not None:
        SAMPLE_TIME = str(override_time)
        for name in RANDOM_TIME_MAP:
            RANDOM_TIME_MAP[name] = str(override_time)


def print_pat_col1(stat):
    stat = stat["task_complexity"]
    n_op = stat["n_op"]
    n_qualifier = stat["n_qualifier"]
    # n_qualifier_avg = (int)(n_qualifier / n_op)
    n_qualifier_goal = stat["n_qualifier_goal"]
    return [safe_print_int(n_op), safe_print_int(n_qualifier), safe_print_int(n_qualifier_goal)]

def print_pat_col2(stat):
    stat = stat["result_complexity"]
    n_var = stat["n_var"]
    n_obs = stat["n_obs"]
    n_gen = stat["n_gen"]
    n_assert = stat["n_assert"]
    return [safe_print_int(n_var),
            safe_print_int(n_gen + n_obs),
            safe_print_int(n_assert)]

def print_pat_col3(stat):
    return [ print_tries(stat["syn_ratio"]), print_tries(stat["random_ratio"]), 
    safe_print_float(stat["random_time"])]
    # return [ print_tries(stat["syn_ratio"]), print_tries(stat["random_ratio"]), print_tries(stat["default_ratio"])]

def print_table_complexity(stat):
    stat = stat["task_complexity"]
    n_op = stat["n_op"]
    n_qualifier = stat["n_qualifier"]
    n_qualifier_goal = stat["n_qualifier_goal"]
    return [safe_print_int(n_op), safe_print_int(n_qualifier), safe_print_int(n_qualifier_goal)]

def print_table_algo(statA):
    res_stat = statA["result_complexity"]
    stat = statA["algo_complexity"]
    return [
        safe_print_float(stat["t_total"]),
        safe_print_int(res_stat["n_obs"] + res_stat["n_gen"]),
        safe_print_int(stat["n_forward"] + stat["n_backward"]),
        safe_print_int(stat["n_sat"])]

def print_pat_col4(statA):
    stat = statA["algo_complexity"]
    return [
        safe_print_float(stat["t_total"]),
        safe_print_int(stat["n_sat"]),    
        safe_print_int(stat["n_forward"] + stat["n_backward"]) ]

hat = ["Set", "Stack", "Graph", "Filesystem", "NFA", "SetSimple", "StackSimple", "GraphSimple", "FilesystemSimple", "NFASimple"]
ifc = ["IFCStore", "IFCAdd", "IFCLoad", "IFCStoreSimple", "IFCAddSimple", "IFCLoadSimple"]
stlc = ["DeBruijn1", "DeBruijn2", "DeBruijn1Simple", "DeBruijn2Simple"]
hashtable = ["HashTable", "HashTableSimple"]
monkeydb = ["Shopping", "CartCC", "Courseware", "CoursewareCC", "Twitter", "TwitterCC", "SmallbankCC", "TreiberStack", "TreiberStackCC", "ShoppingSimple", "CartCCSimple", "CoursewareSimple", "CoursewareCCSimple", "TwitterSimple", "TwitterCCSimple", "SmallbankSimple", "SmallbankCCSimple", "TreiberStackSimple", "TreiberStackCCSimple"]


def manual_label(name):
    if name in ifc:
        return "^{\\dagger}"
    elif name in stlc:
        return "^{\\dagger}"
    elif name in hashtable:
        return "^{\\dagger}"
    elif name in monkeydb:
        return "^{\\dagger}"
    else:
        return ""

monkeydb_ratio = {
    "Shopping": 20.0,
    "Courseware": 57.5,
    "Twitter": 6.3,
    "TreiberStack": 3.7,
}

oltpbench = ["Smallbank"]

def pp_benchname(name):
    postfix=""
    if name in hat:
        postfix = "\\cite{ZYDJ24}"
    elif name in ifc:
        postfix = "\\cite{pbt-ifc}"
    elif name in stlc:
        postfix = "\\cite{CoverageType}"
    elif name in hashtable:
        postfix = "\\cite{OcamlMulticorePBT}"
    elif name in monkeydb:
        postfix = "\\cite{MonkeyDB}"
    elif name in oltpbench:
        postfix = "\\cite{OLTPBench}"
    return textsf(name) + postfix

def print_pat_col(name, stat):
    col = print_table_complexity(stat) + print_pat_col2(stat) + print_pat_col3(stat) + print_pat_col4(stat)
    col = [pp_benchname(name)] + col
    print (" & ".join(col) + "\\\\")

def print_table_compare(name, stat):
    return [ print_tries(stat["syn_ratio"]), print_tries_label(stat["random_ratio"], manual_label(name))]

def print_tabel1_col(name, stat):
    col = print_table_complexity(stat) + print_table_compare(name, stat) + print_table_algo(stat)
    col = [pp_benchname(name), discription(name)] + col
    print (" & ".join(col) + "\\\\")

def load_stat():
    jmap = {}
    for name in benchmarks:
        stat_file = "stat/.{}.json".format(task_name(name))
        try:
            with open(stat_file, "r") as f:
                jmap[name] = json.load(f)
        except Exception as e:
            jmap[name] = {
                "task_complexity": {
                    "n_op": None,
                    "n_qualifier": None,
                },
                "result_complexity": {
                    "n_var": None,
                    "n_gen": None,
                    "n_obs": None,
                    "n_assert": None,
                },
                "algo_complexity": {
                    "t_total": None,
                    "n_sat": None,
                    "n_forward": None,
                    "n_backward": None,
                },
            }
    return jmap

def print_cols(benchnames, stat):
    random_stat = load_eval_stat(random_stat_file)
    syn_stat = load_eval_stat(syn_stat_file)
    default_stat = load_eval_stat(default_stat_file)
    for name in benchnames:
        if task_name(name) in random_stat:
            stat[name]["random_ratio"] = random_stat[task_name(name)][0]
            stat[name]["random_time"] = random_stat[task_name(name)][1]
        else:
            stat[name]["random_ratio"] = None
            stat[name]["random_time"] = None
        if task_name(name) in syn_stat:
            stat[name]["syn_ratio"] = syn_stat[task_name(name)][0]
            stat[name]["syn_time"] = syn_stat[task_name(name)][1]
        else:
            stat[name]["syn_ratio"] = None
            stat[name]["syn_time"] = None
        if task_name(name) in default_stat:
            stat[name]["default_ratio"] = default_stat[task_name(name)][0]
            stat[name]["default_time"] = default_stat[task_name(name)][1]
        else:
            stat[name]["default_ratio"] = None
            stat[name]["default_time"] = None
    i = len(benchnames)
    for name in benchnames:
        print_pat_col(name, stat[name])
        i = i - 1
        if i > 0:
            print("\\midrule")
        if name in ["NFA", "DeBruijn2"]:
            print("\\midrule")
    print("\\bottomrule\n\\end{tabular}\n\n")
    return

def table1(benchnames, stat):
    random_stat = load_eval_stat(random_stat_file)
    syn_stat = load_eval_stat(syn_stat_file)
    default_stat = load_eval_stat(default_stat_file)
    for name in benchnames:
        if name in monkeydb_ratio:
            stat[name]["random_ratio"] = monkeydb_ratio[name]
        elif name == "HashTable":
            stat[name]["random_ratio"] = 2.5
        elif task_name(name) in random_stat:
            stat[name]["random_ratio"] = random_stat[task_name(name)][0]
        else:
            stat[name]["random_ratio"] = None
        if task_name(name) in syn_stat:
            stat[name]["syn_ratio"] = syn_stat[task_name(name)][0]
        else:
            stat[name]["syn_ratio"] = None
            # if task_name(name) in default_stat:
            #     stat[name]["default_ratio"] = default_stat[task_name(name)][0]
            # else:
            #     stat[name]["default_ratio"] = None
    i = len(benchnames)
    for name in benchnames:
        print_tabel1_col(name, stat[name])
        i = i - 1
        if i > 0:
            print("\\midrule")
        if name in ["Filesystem", "DeBruijn2"]:
            print("\\midrule")
    print("\\bottomrule\n\\end{tabular}\n\n")
    return


# Markdown variants (plain text, no LaTeX)
def _md_int(i):
    return "-" if i is None else str(i)

def _md_float(i):
    return "-" if i is None else "{:.2f}".format(i)

def _md_tries(ratio):
    if ratio is None:
        return "-"
    elif not math.isfinite(ratio) or ratio < 0.1:
        return "Timeout"
    else:
        return "{:.1f}".format(ratio)

def manual_label_md(name):
    if name in ifc:
        return " †"
    elif name in stlc:
        return " †"
    elif name in hashtable:
        return " †"
    elif name in monkeydb:
        return " †"
    else:
        return ""

def _md_tries_label(ratio, label):
    base = _md_tries(ratio)
    if base == "Timeout" and label:
        return "Timeout" + label
    return base

def _md_benchname(name):
    return name

def _md_discription(name):
    import re
    d = discription_dict.get(name, "").replace("\n  ", " ")
    d = re.sub(r'\$\\Code\{([^}]+)\}\$', r'\1', d)  # $\Code{X}$ -> X
    return d

def _table1_row_md(name, stat):
    complexity = print_table_complexity(stat)
    compare = print_table_compare(name, stat)
    algo = print_table_algo(stat)
    # Use MD-formatted values
    c_md = [_md_int(stat["task_complexity"]["n_op"]), _md_int(stat["task_complexity"]["n_qualifier"]), _md_int(stat["task_complexity"]["n_qualifier_goal"])]
    compare_md = [_md_tries(stat["syn_ratio"]), _md_tries_label(stat["random_ratio"], manual_label_md(name))]
    res_stat = stat["result_complexity"]
    algo_stat = stat["algo_complexity"]
    algo_md = [_md_float(algo_stat["t_total"]), _md_int(res_stat["n_obs"] + res_stat["n_gen"]), _md_int(algo_stat["n_forward"] + algo_stat["n_backward"]), _md_int(algo_stat["n_sat"])]
    return [_md_benchname(name), _md_discription(name)] + c_md + compare_md + algo_md

def table1_md(benchnames, stat):
    random_stat = load_eval_stat(random_stat_file)
    syn_stat = load_eval_stat(syn_stat_file)
    for name in benchnames:
        if name in monkeydb_ratio:
            stat[name]["random_ratio"] = monkeydb_ratio[name]
        elif name == "HashTable":
            stat[name]["random_ratio"] = 2.5
        elif task_name(name) in random_stat:
            stat[name]["random_ratio"] = random_stat[task_name(name)][0]
        else:
            stat[name]["random_ratio"] = None
        if task_name(name) in syn_stat:
            stat[name]["syn_ratio"] = syn_stat[task_name(name)][0]
        else:
            stat[name]["syn_ratio"] = None
    headers = ["Benchmark(Name)", "Benchmark(Property)", "#op", "#qualifier(uHAT)", "#qualifier(goal)", "#Num.Ex(Clouseau)", "#Num.Ex(Baseline)", "t_total", "#evt", "#refine", "#SMT"]
    header_row = "| " + " | ".join(headers) + " |"
    sep_row = "| " + " | ".join(["---"] * len(headers)) + " |"
    print(header_row)
    print(sep_row)
    for name in benchnames:
        row = _table1_row_md(name, stat[name])
        # Escape pipe in description for markdown
        row[1] = row[1].replace("|", "\\|")
        print("| " + " | ".join(str(c) for c in row) + " |")
    print()

def do_syn(candidate_num="1"):
    for bench_name in benchmarks:
        cmd = cmd_prefix + ["do-syn", task_name(bench_name), task_dir(bench_name), candidate_num]
        invoc_cmd(cmd)
    return

def do_parse():
    for bench_name in benchmarks:
        cmd = cmd_prefix + ["do-parse", task_name(bench_name), task_dir(bench_name)]
        invoc_cmd(cmd)
    return

def run_syn():
    for bench_name in benchmarks:
        # count (int): <=0 means None; time (float): <=0 means None
        cmd = cmd_prefix + ["sample-syn", task_name(bench_name), SAMPLE_COUNT, SAMPLE_TIME]
        invoc_cmd(cmd)
    return

def run_random():
    for bench_name in benchmarks:
        if bench_name not in monkeydb:
            # count (int): <=0 means None; time (float): <=0 means None
            cmd = cmd_prefix + ["sample-random", task_name(bench_name), "0", RANDOM_TIME_MAP[bench_name]]
            invoc_cmd(cmd)
    return

def run_default():
    exit()

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
    parser = argparse.ArgumentParser(description='Run ADT benchmarks')
    parser.add_argument('command', nargs='?', default='all', help='Command to run (syn, runsyn, runrandom, etc.)')
    parser.add_argument('-b', '--benchmarks', type=str, help='Comma-separated list of benchmarks to run')
    parser.add_argument('-n', '--number', type=int, help='Override synthesis sample count for fast run mode')
    parser.add_argument('-t', '--time', type=float, help='Override time limit (seconds) for runsyn and runrandom')
    parser.add_argument('-c', '--candidate', type=str, default="1", help='Number of candidates for synthesis')
    parser.add_argument('-v', '--verbose', action='store_true', default=False, help='Enable verbose output (print commands)')
    parser.add_argument('extra_args', nargs='*', help='Extra arguments for specific commands')
    
    args = parser.parse_args()
    common.verbose = args.verbose

    if args.benchmarks:
        parsed = parse_benchmarks(args.benchmarks)
        if parsed:
            benchmarks = parsed

    build_and_copy_exe()
    init_config(args.number, args.time)

    if args.command == "syn":
        do_syn(args.candidate)
    elif args.command == "runsyn":
        run_syn()
        # j = load_stat()
        # print_cols(benchmarks, j)
    elif args.command == "runrandom":
        run_random()
        # j = load_stat()
        # print_cols(benchmarks, j)
    elif args.command == "parse":
        do_parse()
        # j = load_stat()
        # print_cols(benchmarks, j)
    elif args.command == "show":
        j = load_stat()
        print_cols(benchmarks, j)
    elif args.command == "table1":
        j = load_stat()
        table1(benchmarks, j)
    elif args.command == "table1_md":
        j = load_stat()
        table1_md(benchmarks, j)
    elif args.command == "all":
        do_syn(args.candidate)
        run_syn()
        run_random()
        j = load_stat()
        table1_md(benchmarks, j)
