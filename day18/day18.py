import heapq

input_file = "day18_input.txt"
n, m = 71, 71
with open(input_file) as f:
    pairs = [row.split(",") for row in f.read().split("\n")]
    bytes = [(int(x), int(y)) for [x, y] in pairs]

N = len(bytes)


def set_positions(memory, positions, c):
    for x, y in positions:
        memory[x][y] = c


def print_memory(memory: list[list[str]]) -> None:
    for row in memory:
        for cell in row:
            print(cell, end="")
        print()


def run_simulation(num_bytes: int) -> int:
    memory = [["." for _ in range(m)] for _ in range(n)]

    set_positions(memory, bytes[:num_bytes], "#")

    dirs = [
        (0, 1),
        (0, -1),
        (1, 0),
        (-1, 0),
    ]
    h = [(0, (0, 0), [(0, 0)])]
    visited = {(0, 0)}
    while h:
        curr_dist, (curr_x, curr_y), acc = heapq.heappop(h)
        if curr_x == n - 1 and curr_y == m - 1:
            return curr_dist
        for dx, dy in dirs:
            nx, ny = curr_x + dx, curr_y + dy
            if (
                nx in range(n)
                and ny in range(m)
                and (nx, ny) not in visited
                and memory[nx][ny] == "."
            ):
                heapq.heappush(h, (curr_dist + 1, (nx, ny), acc + [(nx, ny)]))
                visited.add((nx, ny))
    return -1


l, r = 0, N
while l < r:
    m = (l + r) // 2
    sim = run_simulation(m)
    if sim == -1:
        r = m
    else:
        l = m + 1
print(run_simulation(l - 1))
print(run_simulation(l))
print(bytes[l - 1])
