import subprocess
import json
import os
import sys
import time
import re
import logging
from common import benchmarks, cases, prefix

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

from common import benchmarks, cases, prefix

def run(bench):
    path = f"{prefix}/Examples/{bench}"
    output_path = f"{prefix}/{benchmarks[bench]}/PSyn/NewClient.p"
    command = ["dune", "exec", "--", "bin/main.exe", "syn", f"{path}/head.p", f"{path}/spec.p", output_path]
    exec(command)

if __name__ == '__main__':
    for bench in cases:
        run(bench)
