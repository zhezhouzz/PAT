import subprocess
import json
import os
import sys
import time

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

verbose = False

cmd_prefix = ["dune", "exec", "--", "bin/main.exe"]

def invoc_cmd(cmd, cwd=None):
    if (verbose):
        print(" ".join(cmd))
    try:
        subprocess.run(cmd, cwd=cwd)
    except subprocess.CalledProcessError as e:
        print(e.output)


# benchmarks = ["Stack", "HashTable", "Filesystem", "Graph", "NFA", "IFCAdd", "IFCStore", "IFCLoad", "DeBruijn2",  "CartRC", "CartCC", "CoursewareRC", "CoursewareCC", "TwitterRC", "TwitterCC", "SmallbankRC", "SmallbankCC", "TreiberStackRC", "TreiberStackCC"]
benchmarks = ["Stack", "HashTable", "Filesystem", "Graph", "NFA", "IFCAdd", "IFCStore",  "IFCLoad", "DeBruijn1", "DeBruijn2", "Shopping", "Courseware", "Twitter", "Smallbank"]
# benchmarks = ["Shopping", "Courseware", "Twitter", "Smallbank"]
# benchmarks  = ["Filesystem", "Graph",  "NFA", "IFCAdd", "IFCStore",  "IFCLoad"]
# benchmarks  = ["DeBruijn1"]

discription_dict = {
    "Stack": "All pushed values should be popped.",
    "HashTable": "Updates must be visible to all threads.",
    "Filesystem": "No access allowed after parent path deletion.",
    "Graph": "A connected graph with at least $3$ nodes.",
    "NFA": "A NFA with at least $3$ edges.",
    "IFCAdd": "A IFC program that contains $\\Code{Add}$ command.",
    "IFCStore": "A IFC program that contains $\\Code{Store}$ command.",
    "IFCLoad": "A IFC program that contains $\\Code{Load}$ command.",
    "DeBruijn1": "A $\\Int$ typed De Bruijn STLC program.",
    "DeBruijn2": "A $\\Int$ typed De Bruijn STLC program and all lambda variables should be used.",
    "Shopping": "Added items should be visible in the cart.",
    "Courseware": "Courses enrolled by students should be visible.",
    "Twitter": "Posted tweets are visible to all followers.",
    "Smallbank": "Updated balance should be visible.",
    "TreiberStack": "Pushed value can be popped.",
}

def discription(name):
    return "{\\scriptsize " + discription_dict[name] + "}"

task_name_dict = {
    "Stack": "stack",
    "Graph": "graph",
    "Filesystem": "filesystem",
    "NFA": "nfa",
    "IFCStore": "ifc_store",
    "IFCAdd": "ifc_add",
    "IFCLoad": "ifc_load",
    "DeBruijn1": "stlc1",
    "DeBruijn2": "stlc2",
    "HashTable": "hashtable",
    "CartRC": "cart_rc",
    "CartCC": "cart_cc",
    "Shopping": "cart_cc",
    "CoursewareRC": "courseware_rc",
    "CoursewareCC": "courseware_cc",
    "Courseware": "courseware_cc",
    "TwitterRC": "twitter_rc",
    "TwitterCC": "twitter_cc",
    "Twitter": "twitter_cc",
    "SmallbankRC": "smallbank_rc",
    "SmallbankCC": "smallbank_cc",
    "Smallbank": "smallbank_cc",
    "TreiberStackRC": "treiber_stack_rc",
    "TreiberStackCC": "treiber_stack_cc",
    "TreiberStack": "treiber_stack_cc",
}

task_dir_dict = {
    "Stack": "ADT/stack_spec.ml",
    "Graph": "ADT/graph_spec.ml",
    "Filesystem": "ADT/filesystem_spec.ml",
    "NFA": "ADT/nfa_spec.ml",
    "IFCStore": "ADT/ifc_spec.ml",
    "IFCAdd": "ADT/ifc_spec.ml",
    "IFCLoad": "ADT/ifc_spec.ml",
    "DeBruijn1": "ADT/stlc_spec_simple.ml",
    "DeBruijn2": "ADT/stlc_spec_moti.ml",
    "HashTable": "ADT/hashtable_spec.ml",
    "CartRC": "MonkeyDB/cart_rc_spec.ml",
    "CartCC": "MonkeyDB/cart_cc_spec.ml",
    "Shopping": "MonkeyDB/cart_cc_spec.ml",
    "CoursewareRC": "MonkeyDB/courseware_rc_spec.ml",
    "CoursewareCC": "MonkeyDB/courseware_cc_spec.ml",
    "Courseware": "MonkeyDB/courseware_cc_spec.ml",
    "TwitterRC": "MonkeyDB/twitter_rc_spec.ml",
    "TwitterCC": "MonkeyDB/twitter_cc_spec.ml",
    "Twitter": "MonkeyDB/twitter_cc_spec.ml",
    "SmallbankRC": "MonkeyDB/smallbank_rc_spec.ml",
    "SmallbankCC": "MonkeyDB/smallbank_cc_spec.ml",
    "Smallbank": "MonkeyDB/smallbank_cc_spec.ml",
    "TreiberStackRC": "MonkeyDB/treiber_stack_rc_spec.ml",
    "TreiberStackCC": "MonkeyDB/treiber_stack_cc_spec.ml",
    "TreiberStack": "MonkeyDB/treiber_stack_cc_spec.ml",
}

def task_name(name):
    return task_name_dict[name]

def task_dir(name):
    return "benchmarks/" + task_dir_dict[name]

def syn_num_map(name):
    return 500

def default_num_map(name):
    return 2000

dict = {
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
    "CartRC": "1800",
    "CartCC": "1800",
    "Shopping": "1800",
    "CoursewareRC": "1800",
    "CoursewareCC": "1800",
    "Courseware": "1800",
    "TwitterRC": "1800",
    "TwitterCC": "1800",
    "Twitter": "1800",
    "SmallbankRC": "1800",
    "SmallbankCC": "1800",
    "Smallbank": "1800",
    "TreiberStackRC": "1800",
    "TreiberStackCC": "1800",
    "TreiberStack": "1800",
}

def random_num_map(name):
    return dict[name]

# import re
# def safe_print(s):
#     return re.sub(r"_", "\_", s)

def safe_print_int(i):
    if i is None:
        return "-"
    else:
        return "${}$".format(i)

def raw_safe_print_time(i):
    if i is None:
        return "-"
    else:
        return "{:.2f}".format(i)

def safe_print_float(i):
    if i is None:
        return "-"
    else:
        return "${:.2f}$".format(i)

def textsf(content: str):
    return "\\textsf{" + content + "}"

def textbf(content: str):
    return "\\textbf{" + content + "}"

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

def print_tries(ratio):
    if ratio is None:
        return "-"
    elif ratio == 0.0:
        return "{\\tiny Timeout}"
    else:
        return "${:.1f}$".format(ratio)

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

def print_table_compare(stat):
    return [ print_tries(stat["syn_ratio"]), print_tries(stat["random_ratio"])]

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

hat = ["Stack", "Graph", "Filesystem", "NFA"]
ifc = ["IFCStore", "IFCAdd", "IFCLoad"]
stlc = ["DeBruijn1", "DeBruijn2"]
hashtable = ["HashTable"]
monkeydb = ["Shopping", "CartRC", "CartCC", "Courseware", "CoursewareRC", "CoursewareCC", "Twitter", "TwitterRC", "TwitterCC", "Smallbank", "SmallbankRC", "SmallbankCC", "TreiberStack", "TreiberStackRC", "TreiberStackCC"]

monkeydb_ratio = {
    "Shopping": 20.0,
    "Courseware": 57.5,
    "Twitter": 6.3,
    "Smallbank": 2.7,
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
    return textsf(name) + postfix

def print_pat_col(name, stat):
    col = print_table_complexity(stat) + print_pat_col2(stat) + print_pat_col3(stat) + print_pat_col4(stat)
    col = [pp_benchname(name)] + col
    print (" & ".join(col) + "\\\\")

def print_tabel1_col(name, stat):
    col = print_table_complexity(stat) + print_table_compare(stat) + print_table_algo(stat)
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

def load_eval_stat(filename):
    if not os.path.exists(filename):
        with open(filename, 'w') as f:
            f.write("{}")
    with open (filename, "r") as f:
        data = json.load(f)
    return data

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
        if name in ["NFA", "DeBruijn2"]:
            print("\\midrule")
    print("\\bottomrule\n\\end{tabular}\n\n")
    return

def do_syn():
    for bench_name in benchmarks:
        cmd = cmd_prefix + ["do-syn", task_name(bench_name), task_dir(bench_name)]
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
    if len(sys.argv) > 1:
        arg = sys.argv[1]
        if arg == "syn":
            do_syn()
        elif arg == "runsyn":
            run_syn()
            j = load_stat()
            print_cols(benchmarks, j)
        elif arg == "runrandom":
            run_random()
            j = load_stat()
            print_cols(benchmarks, j)
        elif arg == "parse":
            do_parse()
            j = load_stat()
            print_cols(benchmarks, j)
        elif arg == "show":
            j = load_stat()
            print_cols(benchmarks, j)
        elif arg == "table1":
            j = load_stat()
            table1(benchmarks, j)
    else:
        do_syn()
        run_syn()
        run_random()
        # run_default()
        j = load_stat()
        print_cols(benchmarks, j)
