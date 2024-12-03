# Part 1
# with open("day2_input.txt") as f:
#     data = f.readlines()
#     safe = 0
#     for line in data:
#         levels = [int(x) for x in line.split(" ")]
#         monotonic = all(a <= b for a, b in zip(levels, levels[1:])) or all(
#             a >= b for a, b in zip(levels, levels[1:])
#         )
#         adj = all(1 <= abs(a - b) <= 3 for a, b in zip(levels, levels[1:]))
#         if monotonic and adj:
#             safe += 1
#     print(safe)


# Part 2
def is_safe(levels: list[int]) -> bool:
    monotonic = all(a <= b for a, b in zip(levels, levels[1:])) or all(
        a >= b for a, b in zip(levels, levels[1:])
    )
    adj = all(1 <= abs(a - b) <= 3 for a, b in zip(levels, levels[1:]))
    return monotonic and adj


with open("day2_input.txt") as f:
    data = f.readlines()
    safe = 0
    for line in data:
        levels = [int(x) for x in line.split(" ")]
        n = len(levels)
        if is_safe(levels) or any(
            is_safe(levels[:i] + levels[i + 1 :]) for i in range(n)
        ):
            safe += 1
    print(safe)
