#!/usr/bin/env python3

import subprocess
import os


def create_divider(test_num):
    divider = "=" * 20
    return f"{divider} TEST {test_num} {divider} \\n \\n \\n"


# Create commands to be executed in SML
sml_commands = [
    'CM.make "sources.cm";',
    *[f'print "{create_divider(i)}"; Parse.parse "tests/test{i}.tig"; print "\\n \\n \\n";' for i in range(1, 55)],
    'OS.Process.exit(0);'
]

# Join commands with semicolons
command_string = ' '.join(sml_commands)

# Create the complete command to run SML with the commands
full_command = f'sml << EOF\n{command_string}\nEOF'

# Run the command and capture output
try:
    # Open a file to write the output
    with open('_parsing_output.txt', 'w') as output_file:
        result = subprocess.run(
            full_command,
            shell=True,
            text=True,
            stdout=output_file,
            stderr=subprocess.STDOUT
        )
    print("Parsing complete. Check parsing_output.txt for results.")
except Exception as e:
    print(f"An error occurred: {e}")

