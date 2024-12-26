class Lock:
    def __init__(self, heights: list[int]) -> None:
        self.heights = heights


class Key:
    def __init__(self, heights: list[int]) -> None:
        self.heights = heights


def compatible(lock: Lock, key: Key) -> bool:
    return all(a + b <= 5 for a, b in zip(lock.heights, key.heights))


locks, keys = [], []
filename = "day25_input.txt"
with open(filename) as f:
    for schematic in f.read().split("\n\n"):
        grid = [list(row) for row in schematic.split("\n")]
        cols = list(zip(*grid))
        heights = [sum(1 for c in col if c == "#") - 1 for col in cols]
        if grid[0][0] == "#":
            locks.append(Lock(heights))
        else:
            keys.append(Key(heights))

print(len(locks), len(keys))

compatible_pairs = 0
for lock in locks:
    for key in keys:
        if compatible(lock, key):
            compatible_pairs += 1
print(compatible_pairs)
