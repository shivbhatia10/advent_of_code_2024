from heapq import *

input_file = "day16_input.txt"

with open(input_file) as f:
    grid = [list(row) for row in f.read().split("\n")]


def find_char(c, grid):
    n, m = len(grid), len(grid[0])
    for i in range(n):
        for j in range(m):
            if grid[i][j] == c:
                return (i, j)


def rotate_right(dx, dy):
    return -dy, dx


def rotate_left(dx, dy):
    return dy, -dx


(sx, sy) = find_char("S", grid)
(ex, ey) = find_char("E", grid)

print(sx, sy)
print(ex, ey)

dx, dy = 0, 1

h = [(0, sx, sy, dx, dy, [(sx, sy)])]
visited = set()
min_dist = None
best_tiles = set()
while h:
    # print(h)
    curr_dist, curr_x, curr_y, curr_dx, curr_dy, acc = heappop(h)
    if (curr_x, curr_y) == (ex, ey):
        if min_dist == None or curr_dist == min_dist:
            min_dist = curr_dist
            best_tiles.update(acc)
    visited.add((curr_x, curr_y, curr_dx, curr_dy))
    nx, ny = curr_x + curr_dx, curr_y + curr_dy
    if grid[nx][ny] != "#" and (nx, ny, curr_dx, curr_dy) not in visited:
        heappush(h, (curr_dist + 1, nx, ny, curr_dx, curr_dy, acc + [(nx, ny)]))
    rx, ry = rotate_right(curr_dx, curr_dy)
    if (curr_x, curr_y, rx, ry) not in visited:
        heappush(h, (1000 + curr_dist, curr_x, curr_y, rx, ry, acc))
    lx, ly = rotate_left(curr_dx, curr_dy)
    if (curr_x, curr_y, lx, ly) not in visited:
        heappush(h, (1000 + curr_dist, curr_x, curr_y, lx, ly, acc))

print(len(best_tiles))
