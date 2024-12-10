import re

with open("day3_input.txt") as f:
    data = f.readlines()
    all_lines = "".join(data)

    # Part 1
    valid_muls = re.findall(r"mul\((\d+),(\d+)\)", all_lines)
    res = sum(int(a) * int(b) for (a, b) in valid_muls)
    print(res)

    # Part 2
    all_lines = "do()" + all_lines
    dos_and_donts = list(re.finditer(r"(do)(n't)?\(\)", all_lines))
    enabled_lines = []
    for a, b in zip(dos_and_donts, dos_and_donts[1:]):
        if a.group() == "do()":
            enabled_lines.append(all_lines[a.end() : b.start()])
    if dos_and_donts[-1].group() == "do()":
        enabled_lines.append(all_lines[dos_and_donts[-1].end() :])
    all_enabled_lines = "".join(enabled_lines)
    valid_muls = re.findall(r"mul\((\d+),(\d+)\)", all_enabled_lines)
    res = sum(int(a) * int(b) for (a, b) in valid_muls)
    print(res)
