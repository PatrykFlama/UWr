import os
import subprocess

program_name = 'main'

# ulimit -v 32000

cwd = os.path.abspath(os.getcwd())
dir_in = cwd + "/tests/in"
dir_out = cwd + "/tests/out"

files = len(os.listdir(dir_in))
print(files, "tests")
file_cnt = 0

for infile in os.listdir(dir_in):
    file_cnt += 1
    if(file_cnt%300 == 0): print("progress:", file_cnt/files)

    if not(infile.endswith('.txt') or infile.endswith('.in')):
        continue

    cnt = 0
    with open(os.path.join(dir_out, infile.replace('in', 'out'))) as outf:
        try:
            p = subprocess.Popen([cwd + '/' + program_name], stdin=open(os.path.join(dir_in, infile), "r"), stdout=subprocess.PIPE)
        except:
            print("CRASH:", infile.replace('in', ''))
            continue

        while True:
            program_line = p.stdout.readline().rstrip().decode("utf-8")
            expected_line = outf.readline().rstrip()

            if not program_line:
                break

            if(expected_line != program_line):
                print("ERR:", infile.replace('in', ''), "line", cnt)
                print("PROG:", program_line)
                print("EXP: ", expected_line)
            cnt += 1
