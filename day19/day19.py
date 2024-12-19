from functools import cache

input_filename = "day19_input.txt"

with open(input_filename) as f:
    lines: list[str] = f.read().split("\n")
    patterns: set[str] = set(lines[0].split(", "))
    towels: list[str] = lines[2:]


@cache
def is_possible(towel: str, acc: str) -> bool:
    if len(towel) == 0:
        return acc in patterns
    acc = acc + towel[0]
    if acc in patterns:
        return is_possible(towel[1:], acc) or is_possible(towel[1:], "")
    return is_possible(towel[1:], acc)


@cache
def get_num_possible_ways(towel: str, acc: str) -> int:
    if len(towel) == 0:
        return 1 if acc in patterns else 0
    acc = acc + towel[0]
    if acc in patterns:
        return get_num_possible_ways(towel[1:], acc) + get_num_possible_ways(
            towel[1:], ""
        )
    return get_num_possible_ways(towel[1:], acc)


print(patterns)

res = 0
for towel in towels:
    possible = is_possible(towel, "")
    print(f"Towel: {towel}, Possible: {possible}")
    if possible:
        res += 1
print(f"Total possible: {res}")

res2 = 0
for towel in towels:
    possible_ways = get_num_possible_ways(towel, "")
    print(f"Towel: {towel}, Possible ways: {possible_ways}")
    res2 += possible_ways
print(f"Total possible ways: {res2}")
