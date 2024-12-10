from functools import cache


dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

with open("day10_input.txt") as f:
    grid = [[int(c) for c in row] for row in f.read().split("\n")]
    n, m = len(grid), len(grid[0])

    # Part 1
    @cache
    def reachable_peaks(i: int, j: int) -> set[tuple[int, int]]:
        if grid[i][j] == 9:
            return {(i, j)}
        curr = grid[i][j]
        next_step = curr + 1
        res = set()
        for dx, dy in dirs:
            x, y = i + dx, j + dy
            if x in range(n) and y in range(m) and grid[x][y] == next_step:
                res |= reachable_peaks(x, y)
        return res

    res = 0
    for i in range(n):
        for j in range(m):
            # print(reachable_peaks(i, j), end=" ")
            if grid[i][j] == 0:
                res += len(reachable_peaks(i, j))
    print(res)

    # Part 2
    def ways_to_peak(i: int, j: int) -> int:
        if grid[i][j] == 9:
            return 1
        curr = grid[i][j]
        next_step = curr + 1
        res = 0
        for dx, dy in dirs:
            x, y = i + dx, j + dy
            if x in range(n) and y in range(m) and grid[x][y] == next_step:
                res += ways_to_peak(x, y)
        return res
    
    res = 0
    for i in range(n):
        for j in range(m):
            if grid[i][j] == 0:
                res += ways_to_peak(i, j)
    print(res)
