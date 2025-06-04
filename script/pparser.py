import re
import sys
import os

def parse_global_function(lines, i):
    restb = 0
    can_break = False
    while (i < len(lines)):
        # print(restb, lines[i])
        if lines[i].count('{') > 0:
            can_break = True
        restb = restb + lines[i].count('{') - lines[i].count('}')
        if can_break and restb == 0:
            return i
        i = i + 1
    print("Error")
    exit(1)

def parse_test(lines, i):
    restb = 1
    while (i < len(lines)):
        if ";" in lines[i]:
            return i
        i = i + 1
    print("Error")
    exit(1)

def is_external_global_function(txt):
    matches = re.finditer(r"^fun.*;", txt)
    for matchNum, match in enumerate(matches, start=1):
        return True
    return False

def is_global_function(txt):
    matches = re.finditer(r"^fun.*", txt)
    for matchNum, match in enumerate(matches, start=1):
        return True
    return False

def is_param(txt):
    matches = re.finditer(r"^param.*", txt)
    for matchNum, match in enumerate(matches, start=1):
        return True
    return False

def is_machine(txt):
    matches = re.finditer(r"^machine.*", txt)
    for matchNum, match in enumerate(matches, start=1):
        return True
    return False

def is_spec(txt):
    matches = re.finditer(r"^spec.*", txt)
    for matchNum, match in enumerate(matches, start=1):
        return True
    return False

def is_module(txt):
    matches = re.finditer(r"^module.*", txt)
    for matchNum, match in enumerate(matches, start=1):
        return True
    return False

def is_test(txt):
    matches = re.finditer(r"^test.*$", txt)
    for matchNum, match in enumerate(matches, start=1):
        return True
    return False

def load_p_file(path):
    with open(path, 'r') as file:
        i = 0
        lines = []
        for l in file:
            lines.append(l.rstrip('\n'))
        res = []
        while (i < len(lines)):
            # print(lines[i])
            if len(lines[i].strip()) == 0:
                i = i + 1
                continue
            if (is_external_global_function(lines[i])):
                i = i + 1
                continue
            elif (is_global_function(lines[i])):
                # print("isFun")
                i = parse_global_function(lines, i)
            elif (is_spec(lines[i])):
                i = parse_global_function(lines, i)
            elif (is_machine(lines[i])):
                i = parse_global_function(lines, i)
            elif (is_test(lines[i])):
                i = parse_test(lines, i)
            elif (is_module(lines[i])):
                i = parse_test(lines, i)
            elif (is_param(lines[i])):
                i = parse_test(lines, i)
            else:
                res.append(lines[i])
            i = i + 1
    return res

def rec_load_p_file(res, path):
    if os.path.isdir(path):
        for name in os.listdir(path):
            res = rec_load_p_file(res, path + "/" + name)
        return res
    elif os.path.isfile(path):
        if os.path.splitext(path)[1] == ".p":
            return res + load_p_file(path)
        return res
    else:
        return res

from common import benchmarks, cases, prefix

if __name__ == '__main__':
    for bench in cases:
        path = prefix + "/" + benchmarks[bench]
        result = rec_load_p_file([], path)
        os.makedirs(f"{prefix}/Examples/{bench}", exist_ok=True)
        output_path = f"{prefix}/Examples/{bench}/head.p"
        with open(output_path, 'w') as file:
            file.write("\n".join(result))
