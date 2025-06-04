import subprocess
import json
import os
import sys
import time
import re
import logging
from common import benchmarks, cases, prefix, client_name_to_tc

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def load_p_trace(trace_file, client_name):
    with open(trace_file, 'r') as file:
        data = json.load(file)
    traces = []
    for trace in data:
        res = {"actual_trace": [], "assertions": {}, "failures": {}}
        for elem in trace:
            if elem["type"] == "SendEvent":
                if (client_name in elem["details"]["sender"]) or (client_name in elem["details"]["target"]):
                    res["actual_trace"].append(elem)
            if elem["type"] == "Print":
                str = elem["details"]["log"]
                regex = r"\[LOCATION\]::([a-zA-Z0-9]+)::([0-9]+)"
                try:
                    print(str)
                    name = re.sub(regex, "\\g<1>", str, 0, re.MULTILINE)
                    loc = re.sub(regex, "\\g<2>", str, 0, re.MULTILINE)
                    loc = int(loc)
                    if name not in res["assertions"]:
                        res["assertions"][name] = []
                    res["assertions"][name].append(loc)
                except:
                    print(str)
                    if name not in res["failures"]:
                        res["failures"][name] = []
                    res["failures"][name].append(elem)
        traces.append(res)
    return traces

def check_valid(trace):
    print(trace["failures"])
    return True

from common import benchmarks, cases, prefix

curp = "~/workspace/P/Bld/Drops/Debug/Binaries/net8.0/p"

def exec(command):
    cmd = " ".join(command)
    print(cmd)
    try:
        result = subprocess.run(cmd, shell=True,
                                capture_output=True,
                                text=True,
                                check=True)
        # print(result.stdout)
        return result
    except subprocess.CalledProcessError as e:
        logger.error(f"Exec Error: {e.cmd}")
        logger.error(f"Error Output: {e.stderr}")
    except subprocess.TimeoutExpired:
        logger.error(f"Timeout: {command}")
    except Exception as e:
        logger.error(f"Unknown: {str(e)}")

def get_timeline(result):
    matches = re.finditer(r"^\.\.\.\.\.\ Explored\ ([0-9]*)\ timelines?$", result.stdout, re.MULTILINE)
    for matchNum, match in enumerate(matches, start=1):
        return match.groups()[0]
    return -1

def run(option, num):
    p_compile_command = [curp, 'compile']
    exec(p_compile_command)
    p_check_command = [curp, "check", "-tc", str(option), "-v", "-s", str(num)]
    result = exec(p_check_command)
    timeline = get_timeline(result)
    print(timeline)
    if timeline == -1:
        exit(1)
    regex = r".* (PCheckerOutput/BugFinding/.*\.json)"
    matches = re.finditer(regex, result.stdout, re.MULTILINE)
    for matchNum, match in enumerate(matches, start=1):
        return (timeline, match.group(1))

# client_name = "KFCClient"
# client_name = "GeneratedClient"
# mode = "Syn"

import argparse

def make_name(name, i):
    return name + str(i);

def calculate_assertion(traces):
    num = len(data)
    m = {}
    for tr in traces:
        for name in tr["assertions"]:
            i = tr["assertions"][name]
            if make_name(name, i) in m:
                m[make_name(name, i)] = m[make_name(name, i)] + 1
            else:
                m[make_name(name, i)] = 0
    for k in m:
        m[k] = float(m[k])/float(num)
    print(m)

def run_and_save(bench, client_name, num):
    mode = client_name_to_tc[bench][client_name]
    dir = prefix + benchmarks[bench]
    os.chdir(dir)
    print(os.getcwd())
    timeline, trace_file = run(mode, num)
    traces = load_p_trace(trace_file, client_name)
    data = {"traces": traces, "timeline": int(timeline), "total": num}
    with open("PCheckerOutput/" + bench + "___" + client_name + "___" + str(num) + '.json', 'w', encoding='utf-8') as f:
        json.dump(data, f)

def run_all(num):
    for bench in cases:
        for client_name in client_name_to_tc:
            run_and_save(bench, client_name, num)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='')
    parser.add_argument('-b', '--benchmark', type=str, help='benchmark name')
    parser.add_argument('-n', '--name', type=str, help='client name')
    parser.add_argument('-s', '--size', type=str, help='number of test')
    args = parser.parse_args()
    if args.size is None:
        num = 1000
    else:
        num = args.size
    if args.benchmark is None:
        for bench in cases:
            for client_name in client_name_to_tc[bench]:
                run_and_save(bench, client_name, num)
    else:
        bench = args.benchmark
        if args.name is None:
            for client_name in client_name_to_tc[bench]:
                run_and_save(bench, client_name, num)
        else:
            client_name = args.name
            run_and_save(bench, client_name, num)

# if __name__ == '__main__':
#     parser = argparse.ArgumentParser(description='')
#     parser.add_argument('-b', '--benchmark', type=str, help='benchmark name')
#     parser.add_argument('-n', '--name', type=str, help='client name')
#     parser.add_argument('-s', '--size', type=str, help='number of test')
#     args = parser.parse_args()
#     bench = args.benchmark
#     client_name = args.name
#     num = args.size
#     run_and_save(bench, client_name, num)
