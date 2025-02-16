#!/usr/bin/env python3

import subprocess
import os
import re

def create_divider(test_num):
    divider = "=" * 20
    return f"{divider} TEST {test_num} {divider} \\n \\n \\n"

directory = '../testcases'

# Create commands to be executed in SML
sml_base_command = 'sml'
commands = []

# Collect test cases
for item in sorted(os.listdir(directory)):
    commands.append(f'sml << EOF\nprint "{create_divider(item[:item.index(".")])}"; CM.make "sources.cm"; Parse.parse "{os.path.join(directory, item)}"; print "\\n \\n \\n";\nEOF')

commands.append('OS.Process.exit(0);')

# Run each command separately
output_file = '_parsing_output.txt'
try:
    with open(output_file, 'w') as out:
        for cmd in commands:
            # subprocess.run(
            #     [sml_base_command],
            #     input=cmd,
            #     text=True,
            #     stdout=out,
            #     stderr=subprocess.STDOUT
            # )
            result = subprocess.run(
                cmd,
                shell=True,
                text=True,
                stdout=out,
                stderr=subprocess.STDOUT
            )
    with open(output_file, 'r') as out:
        txt = out.read()
    #print(txt)
    #print(f'{"=" * 20} .*? {"=" * 20}\n')
    tests = [(m.start(0), m.end(0)) for m in re.finditer(f'{"=" * 20} .*? {"=" * 20}', txt)]
    errs = [m.start(0) for m in re.finditer('uncaught exception TypeCheck', txt)]
    errprints = []
    print('DID ERROR')
    for err in errs:
        for i in range(len(tests) - 1):
            if tests[i][0] <= err and err <= tests[i + 1][0]:
                errprints.append(txt[tests[i][0]:tests[i][1]][26:-21])
                print(txt[tests[i][0]:tests[i][1]][26:-20])
                break
        else:
            errprints.append(txt[tests[-1][0]:tests[-1][1]][26:-21])
            print(txt[tests[-1][0]:tests[-1][1]][26:-20])
    errcases = []
    for item in sorted(os.listdir(directory)):
        with open(os.path.join(directory, item), 'r') as f:
            if 'error' in f.read():
                errcases.append(item[:-4])
    print('SHOULD ERROR')
    for i in sorted(errcases):
        print(i)
    print('ERRORED BUT IS CLEAN')
    print(set(errprints) - set(errcases))
    print('SHOULD ERROR BUT PASSED')
    print(set(errcases) - set(errprints))
    print("Parsing complete. Check _parsing_output.txt for results.")
except Exception as e:
    print(f"An error occurred: {e}")