from collections import defaultdict

filename = "day20_input.txt"

with open(filename) as f:
    grid = [list(row) for row in f.read().split("\n")]

# print(grid)
n, m = len(grid), len(grid[0])

(sx, sy) = [(i, j) for i in range(n) for j in range(m) if grid[i][j] == "S"][0]
(ex, ey) = [(i, j) for i in range(n) for j in range(m) if grid[i][j] == "E"][0]

dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]

dists: dict[tuple[int, int], int | float] = defaultdict(lambda: float("inf"))
dfs = [(ex, ey, 0)]
while dfs:
    curr_x, curr_y, curr_d = dfs.pop()
    dists[(curr_x, curr_y)] = curr_d
    for dx, dy in dirs:
        new_x, new_y = curr_x + dx, curr_y + dy
        if (
            0 <= new_x < n
            and 0 <= new_y < m
            and grid[new_x][new_y] != "#"
            and curr_d + 1 < dists[(new_x, new_y)]
        ):
            dfs.append((new_x, new_y, curr_d + 1))

# print(dists)


def manhatten_distance(x1: int, y1: int, x2: int, y2: int) -> int:
    return abs(x1 - x2) + abs(y1 - y2)


def get_cheats(x: int, y: int, dist: int) -> list[tuple[int, int]]:
    # Find all points within Manhatten distance dist from (x, y), if they don't end in '#'
    cheats = []
    for i in range(-dist, dist + 1, 1):
        j_range = dist - abs(i)
        for j in range(-j_range, j_range + 1, 1):
            if (
                0 <= x + i < n
                and 0 <= y + j < m
                and grid[x + i][y + j] != "#"
                and 2 <= manhatten_distance(x, y, x + i, y + j) <= dist
            ):
                cheats.append((x + i, y + j))
    return cheats


time_saved_counters: dict[int, int] = defaultdict(int)
for i in range(n):
    for j in range(m):
        if grid[i][j] != "#":
            dist_2_cheats = get_cheats(i, j, 20)
            for cheat in dist_2_cheats:
                d = manhatten_distance(i, j, cheat[0], cheat[1])
                time_saved = dists[(i, j)] - dists[cheat] - d
                if time_saved >= 100:
                    time_saved_counters[time_saved] += 1

print(sum(time_saved_counters.values()))
