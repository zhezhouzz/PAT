from common import *
import argparse
import sys

bench_json = []

random_stat_file = "stat/.run_random_p.json"
syn_stat_file = "stat/.run_syn.json"
default_stat_file = "stat/.run_default.json"

p_repo = ""

def mk_local_path(name):
    return "benchmarks/" + name

def mk_header_path(name):
    return mk_local_path(name) + "/HeaderSpec.p"

def mk_spec_path(name, specname):
    return mk_local_path(name) + "/" + specname + ".p"

def mk_output_path(pname):
    return p_repo + "/" + pname + "/PSyn/SynClient.p"

benchmarks = ["Stack", "Set", "Filesystem", "Graph", "NFA", "IFCStore", "IFCAdd", "IFCLoad", "DeBruijn1", "DeBruijn2", "Shopping", "HashTable", "ReaderWriter", "Courseware", "Twitter", "Smallbank"]

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
    "ReaderWriter": "Asynchronous read operations are logically atomic.",
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
    "FilesystemSimple": "filesystem",
    "GraphSimple": "graph",
    "NFASimple": "nfa",
    "IFCStoreSimple": "ifc_store",
    "IFCAddSimple": "ifc_add",
    "IFCLoadSimple": "ifc_load",
    "DeBruijn1Simple": "stlc1",
    "DeBruijn2Simple": "stlc2",
    "HashTableSimple": "hashtable",
    "ReaderWriterSimple": "read_cc",
    "CartCCSimple": "cart_cc",
    "ShoppingSimple": "cart_cc",
    "CoursewareCCSimple": "courseware_cc",
    "CoursewareSimple": "courseware_cc",
    "TwitterCCSimple": "twitter_cc",
    "TwitterSimple": "twitter_cc",
    "SmallbankCCSimple": "smallbank_cc",
    "SmallbankSimple": "smallbank_cc",
    "TreiberStackCCSimple": "treiber_stack_cc",
    "TreiberStackSimple": "treiber_stack_cc",
    "StackSimple": "stack",
    "SetSimple": "set",
    "NFA": "nfa",
    "IFCStore": "ifc_store",
    "IFCAdd": "ifc_add",
    "IFCLoad": "ifc_load",
    "DeBruijn1": "stlc1",
    "DeBruijn2": "stlc2",
    "HashTable": "hashtable",
    "ReaderWriter": "read_cc",
    "CartCC": "cart_cc",
    "Shopping": "cart_cc",
    "CoursewareCC": "courseware_cc",
    "Courseware": "courseware_cc",
    "TwitterCC": "twitter_cc",
    "Twitter": "twitter_cc",
    "SmallbankCC": "smallbank_cc",
    "Smallbank": "smallbank_cc",
    "TreiberStackCC": "treiber_stack_cc",
    "TreiberStack": "treiber_stack_cc",
}

task_dir_dict = {
    "Set": "set_spec.ml",
    "Stack": "stack_spec.ml",
    "Graph": "graph_spec.ml",
    "Filesystem": "filesystem_spec.ml",
    "FilesystemSimple": "filesystem_simp_spec.ml",
    "GraphSimple": "graph_simp_spec.ml",
    "NFASimple": "nfa_simp_spec.ml",
    "IFCStoreSimple": "ifc_simp_spec.ml",
    "IFCAddSimple": "ifc_simp_spec.ml",
    "IFCLoadSimple": "ifc_simp_spec.ml",
    "DeBruijn1Simple": "stlc_spec_simple_simp.ml",
    "DeBruijn2Simple": "stlc_spec_moti_simp.ml",
    "HashTableSimple": "hashtable_simp_spec.ml",
    "ReaderWriterSimple": "MonkeyDB/read_cc_simp_spec.ml",
    "CartCCSimple": "MonkeyDB/cart_cc_simp_spec.ml",
    "ShoppingSimple": "MonkeyDB/cart_cc_simp_spec.ml",
    "CoursewareCCSimple": "MonkeyDB/courseware_cc_simp_spec.ml",
    "CoursewareSimple": "MonkeyDB/courseware_cc_simp_spec.ml",
    "TwitterCCSimple": "MonkeyDB/twitter_cc_simp_spec.ml",
    "TwitterSimple": "MonkeyDB/twitter_cc_simp_spec.ml",
    "SmallbankCCSimple": "MonkeyDB/smallbank_cc_simp_spec.ml",
    "SmallbankSimple": "MonkeyDB/smallbank_cc_simp_spec.ml",
    "TreiberStackCCSimple": "MonkeyDB/treiber_stack_cc_simp_spec.ml",
    "TreiberStackSimple": "MonkeyDB/treiber_stack_cc_simp_spec.ml",
    "StackSimple": "stack_simp_spec.ml",
    "SetSimple": "set_simp_spec.ml",
    "NFA": "nfa_spec.ml",
    "IFCStore": "ifc_spec.ml",
    "IFCAdd": "ifc_spec.ml",
    "IFCLoad": "ifc_spec.ml",
    "DeBruijn1": "stlc_spec_simple.ml",
    "DeBruijn2": "stlc_spec_moti.ml",
    "HashTable": "hashtable_spec.ml",
    "ReaderWriter": "MonkeyDB/read_cc_spec.ml",
    "CartCC": "MonkeyDB/cart_cc_spec.ml",
    "Shopping": "MonkeyDB/cart_cc_spec.ml",
    "CoursewareCC": "MonkeyDB/courseware_cc_spec.ml",
    "Courseware": "MonkeyDB/courseware_cc_spec.ml",
    "TwitterCC": "MonkeyDB/twitter_cc_spec.ml",
    "Twitter": "MonkeyDB/twitter_cc_spec.ml",
    "SmallbankCC": "MonkeyDB/smallbank_cc_spec.ml",
    "Smallbank": "MonkeyDB/smallbank_cc_spec.ml",
    "TreiberStackCC": "MonkeyDB/treiber_stack_cc_spec.ml",
    "TreiberStack": "MonkeyDB/treiber_stack_cc_spec.ml",
}

def task_name(name):
    return task_name_dict[name]

def task_dir(name):
    return "benchmarks/OCamlBench/" + task_dir_dict[name].replace("ADT/", "")

def syn_num_map(name):
    return 500

def default_num_map(name):
    return 2000

dict = {
    "Set": "1",
    "Stack": "1800",
    "Graph": "1800",
    "Filesystem": "1800",
    "NFA": "1800",
    "IFCStore": "1800",
    "IFCAdd": "1800",
    "IFCLoad": "1800",
    "DeBruijn1": "1800",
    "DeBruijn2": "1800",
    "HashTable": "1800",
    "ReaderWriter": "1",
    "CartCC": "1800",
    "Shopping": "1800",
    "CoursewareCC": "1800",
    "Courseware": "1800",
    "TwitterCC": "1800",
    "Twitter": "1800",
    "SmallbankCC": "1800",
    "Smallbank": "1800",
    "TreiberStackCC": "1800",
    "SetSimple": "1",
    "StackSimple": "1800",
    "GraphSimple": "1800",
    "FilesystemSimple": "1800",
    "NFASimple": "1800",
    "IFCStoreSimple": "1800",
    "IFCAddSimple": "1800",
    "IFCLoadSimple": "1800",
    "DeBruijn1Simple": "1800",
    "DeBruijn2Simple": "1800",
    "HashTableSimple": "1800",
    "ReaderWriterSimple": "1",
    "CartCCSimple": "1800",
    "ShoppingSimple": "1800",
    "CoursewareCCSimple": "1800",
    "CoursewareSimple": "1800",
    "TwitterCCSimple": "1800",
    "TwitterSimple": "1800",
    "SmallbankCCSimple": "1800",
    "SmallbankSimple": "1800",
    "TreiberStackCCSimple": "1800",
    "TreiberStackSimple": "1800",
}

def random_num_map(name):
    return dict[name]

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
monkeydb = ["Shopping", "CartCC", "Courseware", "CoursewareCC", "Twitter", "TwitterCC", "Smallbank", "SmallbankCC", "TreiberStack", "TreiberStackCC", "ShoppingSimple", "CartCCSimple", "CoursewareSimple", "CoursewareCCSimple", "TwitterSimple", "TwitterCCSimple", "SmallbankSimple", "SmallbankCCSimple", "TreiberStackSimple", "TreiberStackCCSimple"]


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
    if name == "ReaderWriter":
        return "\\textsf{Transaction}"
    elif name == "Smallbank":
        return "\\textsf{Smallbank}\\cite{OLTPBench}"
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
        elif name == "Smallbank":
            stat[name]["random_ratio"] = None
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

def do_syn():
    for bench_name in benchmarks:
        cmd = cmd_prefix + ["do-syn", task_name(bench_name), task_dir(bench_name), "3"]
        invoc_cmd(cmd)
    return

def do_parse():
    for bench_name in benchmarks:
        cmd = cmd_prefix + ["do-parse", task_name(bench_name), task_dir(bench_name)]
        invoc_cmd(cmd)
    return

def run_syn():
    for bench_name in benchmarks:
        cmd = cmd_prefix + ["sample-syn", task_name(bench_name), "200"]
        print(" ".join(cmd))
        invoc_cmd(cmd)
    return

def run_random():
    for bench_name in benchmarks:
        if bench_name not in monkeydb:
            cmd = cmd_prefix + ["sample-random", task_name(bench_name), dict[bench_name]]
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
    parser.add_argument('extra_args', nargs='*', help='Extra arguments for specific commands')
    
    args = parser.parse_args()
    
    if args.benchmarks:
        parsed = parse_benchmarks(args.benchmarks)
        if parsed:
            benchmarks = parsed

    build_and_copy_exe()

    if args.command == "syn":
        do_syn()
    elif args.command == "runsyn":
        run_syn()
        j = load_stat()
        print_cols(benchmarks, j)
    elif args.command == "syn-one":
        if not args.extra_args:
            print("Error: syn-one requires a benchmark name")
            sys.exit(1)
        name = args.extra_args[0]
        benchmarks = [name]
        do_syn()
    elif args.command == "runsyn-one":
        if not args.extra_args:
            print("Error: runsyn-one requires a benchmark name")
            sys.exit(1)
        name = args.extra_args[0]
        benchmarks = [name]
        run_syn()
        j = load_stat()
        print_cols(benchmarks, j)
    elif args.command == "runrandom":
        run_random()
        j = load_stat()
        print_cols(benchmarks, j)
    elif args.command == "parse":
        do_parse()
        j = load_stat()
        print_cols(benchmarks, j)
    elif args.command == "show":
        j = load_stat()
        print_cols(benchmarks, j)
    elif args.command == "table1":
        j = load_stat()
        table1(benchmarks, j)
    elif args.command == "all":
        do_syn()
        run_syn()
        run_random()
        # run_default()
        j = load_stat()
        print_cols(benchmarks, j)
