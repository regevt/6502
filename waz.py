import subprocess
import pyperclip


def run_process_and_print_output(command):
    res = ""
    counter = 0x400
    result = subprocess.run(command, shell=True, capture_output=True, text=True)
    lines = result.stdout.splitlines()
    for line in lines:
        res += "{}: {}\n".format(f"{counter:04X}", line.rstrip())
        counter += 16
    return res


code = run_process_and_print_output(
    "hexdump -v  -e '16/1 \"%02X \" \"\n\"' ./programs/MontyOnTheRun.bin | awk '{print toupper($0)}'"
)

with open("waz.txt", "w") as f:
    code += "0400 R\n"
    f.write(code)

pyperclip.copy(code)
