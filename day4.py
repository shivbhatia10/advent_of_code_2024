with open("day4_input.txt") as f:
    data = f.readlines()
    target = list("XMAS")
    grid = [list(x.strip()) for x in data]
    n, m = len(grid), len(grid[0])
    dirs = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]

    # Part 1
    count = 0
    for i in range(n):
        for j in range(m):
            if grid[i][j] != "X":
                continue
            for di, dj in dirs:
                k = 0
                while (
                    k < len(target)
                    and i + k * di in range(n)
                    and j + k * dj in range(m)
                    and grid[i + k * di][j + k * dj] == target[k]
                ):
                    k += 1
                count += k == len(target)
    print(count)

    # Part 2
    def is_wing(arr: list[str]) -> bool:
        return arr == list("MAS") or arr == list("SAM")

    count = 0
    for i in range(1, n - 1):
        for j in range(1, m - 1):
            if grid[i][j] != "A":
                continue
            if not is_wing([grid[i - 1][j - 1], grid[i][j], grid[i + 1][j + 1]]):
                continue
            if not is_wing([grid[i - 1][j + 1], grid[i][j], grid[i + 1][j - 1]]):
                continue
            count += 1
    print(count)
