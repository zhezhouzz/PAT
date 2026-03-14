import subprocess
import json
import os
import math

verbose = False
cmd_prefix = ["./main.exe"]

def build_and_copy_exe():
    print("Building project...")
    subprocess.run(["dune", "build"], check=True)
    print("Copying main.exe...")
    subprocess.run(["cp", "_build/default/bin/main.exe", "main.exe"], check=True)
    subprocess.run(["chmod", "+w", "main.exe"], check=True)

def invoc_cmd(cmd, cwd=None):
    if (verbose):
        print(" ".join(cmd))
    try:
        subprocess.run(cmd, cwd=cwd)
    except subprocess.CalledProcessError as e:
        print(e.output)

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

def print_tries(ratio):
    if ratio is None:
        return "-"
    elif not math.isfinite(ratio) or ratio < 0.1:
        return "{\\tiny Timeout}"
    else:
        return "${:.1f}$".format(ratio)

def print_tries_label(ratio, label):
    if ratio is None:
        if label == "":
            return "-"
        else:
            return "{\\tiny Timeout}" + "${}$".format(label)
    elif not math.isfinite(ratio) or ratio < 0.1:
        if label == "":
            return "{\\tiny Timeout}"
        else:
            return "{{\\tiny Timeout}}${}$".format(label)
    else:
        if label == "":
            return "${:.1f}$".format(ratio)
        else:
            return "${:.1f}{}$".format(ratio, label)

def load_eval_stat(filename):
    if not os.path.exists(filename):
        with open(filename, 'w') as f:
            f.write("{}")
    with open (filename, "r") as f:
        data = json.load(f)
    return data

def parse_benchmarks(names_str):
    if not names_str:
        return []
    return [x.strip() for x in names_str.split(',') if x.strip()]
