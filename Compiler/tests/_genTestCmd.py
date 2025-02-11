import os

def list_files_in_directory(directory):
    with open('_testCmds.txt', 'w') as file:
        files = [filename for filename in os.listdir(directory) if filename.startswith("test") and filename.endswith(".tig")]
        sorted_files = sorted(files, key=lambda x: int(x[4:-4]))
        for filename in sorted_files:
            file.write(f"Parse.parse \"tests/{filename}\";\n")

if __name__ == "__main__":
    current_directory = os.path.dirname(os.path.abspath(__file__))
    list_files_in_directory(current_directory)

# cmd + k clears scroll back
