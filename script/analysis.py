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

import argparse

def make_name(name, i):
    return name + str(i);

def calculate_assertion(traces):
    num = len(traces)
    m = {}
    for tr in traces:
        for name in tr["assertions"]:
            # print(name, tr["assertions"][name])
            for i in tr["assertions"][name]:
                if make_name(name, i) in m:
                    m[make_name(name, i)] = m[make_name(name, i)] + 1
                else:
                    m[make_name(name, i)] = 0
    for k in m:
        m[k] = float(m[k])/float(num)
    # print(m)
    return m

def anal(bench, client_name, num):
    dir = prefix + benchmarks[bench]
    os.chdir(dir)
    print(os.getcwd())
    filename = "PCheckerOutput/" + bench + "___" + client_name + "___" + str(num) + '.json'
    print(filename)
    # print(bench)
    # print(client_name)
    # print(num)
    with open(filename, 'r', encoding='utf-8') as f:
        data = json.load(f)
    traces = data["traces"]
    total = data["total"]
    timeline = data["timeline"]
    print(f"{client_name}: {timeline}")
    return calculate_assertion(traces)

def anal_bench(bench, client_names, num):
    res = {}
    feilds = []
    for client_name in client_names:
        res[client_name] = anal(bench, client_name, num)
        for k in res[client_name]:
            if k not in feilds:
                feilds.append(k)
    for k in res:
        for fd in feilds:
            if fd not in res[k]:
                res[k][fd] = 0.0
        res[k] = dict(sorted(res[k].items()))
    print(bench)
    print(res)
    return res

import matplotlib.pyplot as plt
import numpy as np

def plot_bench(bench, client_names, num, data):
    labels = []
    for k in data:
        labels = list(data[k].keys())
        break
    if labels is []:
        exit(1)
    values = {}
    for k in data:
        values[k] = list(data[k].values())

    x = np.arange(len(labels))
    fig, ax = plt.subplots(figsize=(10, 4))
    for k in values:
        if "Random" in k:
            ax.plot(x, values[k], 'o--', label=k)
        else:
            ax.plot(x, values[k], 'o-', label=k)

    ax.set_ylabel('Frequency of assertion check')
    ax.set_title(bench)
    ax.set_xticks(x)
    ax.set_xticklabels(labels)
    ax.legend()

    plt.tight_layout()
    plt.show()

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
            data = anal_bench(bench, client_name_to_tc[bench], num)
            plot_bench(bench, client_name_to_tc[bench], num, data)
    else:
        bench = args.benchmark
        if args.name is None:
            data = anal_bench(bench, client_name_to_tc[bench], num)
            plot_bench(bench, client_name_to_tc[bench], num, data)
        else:
            client_name = args.name
            data = anal_bench(bench, [client_name], num)
            plot_bench(bench, client_name_to_tc[bench], num, data)

# if __name__ == '__main__':
#     parser = argparse.ArgumentParser(description='')
#     parser.add_argument('-b', '--benchmark', type=str, help='benchmark name')
#     parser.add_argument('-n', '--name', type=str, help='client name')
#     parser.add_argument('-s', '--size', type=str, help='number of test')
#     args = parser.parse_args()
#     bench = args.benchmark
#     client_name = args.name
#     num = args.size
#     anal(bench, client_name, num)
