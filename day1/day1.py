from collections import Counter

# Part 1
with open("day1_input.txt") as f:
    data = f.readlines()
    a, b = [], []
    for line in data:
        [x, y] = [int(x) for x in line.split()]
        a.append(x)
        b.append(y)
    print(sum(abs(x - y) for x, y in zip(sorted(a), sorted(b))))

# Part 2
with open("day1_input.txt") as f:
    data = f.readlines()
    a, b = [], []
    for line in data:
        [x, y] = [int(x) for x in line.split()]
        a.append(x)
        b.append(y)
    b_counter = Counter(b)
    print(sum(x * b_counter[x] for x in a))
