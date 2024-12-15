move_to_dir = {"^": (-1, 0), "v": (1, 0), "<": (0, -1), ">": (0, 1)}
input_file = "day15_input.txt"


def find_start(grid):
    for i, row in enumerate(grid):
        for j, cell in enumerate(row):
            if cell == "@":
                return (i, j)
    return (-1, -1)


def print_grid(grid):
    for row in grid:
        print("".join(row))


# Part 1

with open(input_file, "r") as file:
    [grid_flat, moves] = file.read().split("\n\n")
    grid = [list(row) for row in grid_flat.split("\n")]
    moves = "".join(moves.split("\n"))

(px, py) = find_start(grid)


def get_sum_of_box_gps_coords(grid):
    n, m = len(grid), len(grid[1])
    res = 0
    for i in range(n):
        for j in range(m):
            if grid[i][j] == "O":
                res += 100 * i + j
    return res


def attempt_move(curr_x, curr_y, acc, grid, dx, dy):
    nx, ny = curr_x + dx, curr_y + dy
    if grid[nx][ny] == "#":
        return acc[0]
    elif grid[nx][ny] == "O":
        acc.append((nx, ny))
        return attempt_move(nx, ny, acc, grid, dx, dy)
    else:
        acc.append((nx, ny))
        for (ax, ay), (bx, by) in list(zip(acc, acc[1:]))[::-1]:
            grid[bx][by] = grid[ax][ay]
            grid[ax][ay] = "."
        return acc[1]


for move in moves:
    (dx, dy) = move_to_dir[move]
    (px, py) = attempt_move(px, py, [(px, py)], grid, dx, dy)

print(get_sum_of_box_gps_coords(grid))

# Part 2


def get_wider_grid(grid):
    n = len(grid)
    new_grid = [[] for _ in range(n)]
    for i in range(n):
        for cell in grid[i]:
            if cell == "#":
                new_grid[i].append("#")
                new_grid[i].append("#")
            elif cell == "O":
                new_grid[i].append("[")
                new_grid[i].append("]")
            elif cell == ".":
                new_grid[i].append(".")
                new_grid[i].append(".")
            elif cell == "@":
                new_grid[i].append("@")
                new_grid[i].append(".")
    return new_grid


with open(input_file, "r") as file:
    [grid_flat, moves] = file.read().split("\n\n")
    grid = [list(row) for row in grid_flat.split("\n")]
    moves = "".join(moves.split("\n"))

grid = get_wider_grid(grid)


def attempt_move_horizontal(curr_x, curr_y, acc, grid, dy):
    ny = curr_y + dy
    if grid[curr_x][ny] == "#":
        return acc[0]
    elif grid[curr_x][ny] == "[":
        acc.append((curr_x, ny))
        return attempt_move_horizontal(curr_x, ny, acc, grid, dy)
    elif grid[curr_x][ny] == "]":
        acc.append((curr_x, ny))
        return attempt_move_horizontal(curr_x, ny, acc, grid, dy)
    else:
        for ax, ay in acc[::-1]:
            grid[ax][ay + dy] = grid[ax][ay]
            grid[ax][ay] = "."
        return (acc[0][0], acc[0][1] + dy)


def attempt_move_vertical(frontier: set, to_move: list, grid, dx):
    if all(grid[x + dx][y] == "." for (x, y) in frontier):
        for mx, my in to_move[::-1]:
            grid[mx + dx][my] = grid[mx][my]
            grid[mx][my] = "."
        return (to_move[0][0] + dx, to_move[0][1])

    new_frontier = set()
    for x, y in frontier:
        nx = x + dx
        if grid[nx][y] == "#":
            return to_move[0]
        elif grid[nx][y] == "[":
            new_frontier.add((nx, y))
            new_frontier.add((nx, y + 1))
        elif grid[nx][y] == "]":
            new_frontier.add((nx, y))
            new_frontier.add((nx, y - 1))
    to_move += list(new_frontier)
    return attempt_move_vertical(new_frontier, to_move, grid, dx)


def get_sum_of_box_gps_coords_big_box(grid):
    n, m = len(grid), len(grid[1])
    res = 0
    for i in range(n):
        for j in range(m):
            if grid[i][j] == "[":
                res += 100 * i + j
    return res


(px, py) = find_start(grid)

for move in moves:
    (dx, dy) = move_to_dir[move]
    if dx == 0:
        (px, py) = attempt_move_horizontal(px, py, [(px, py)], grid, dy)
    else:
        (px, py) = attempt_move_vertical({(px, py)}, [(px, py)], grid, dx)
print(get_sum_of_box_gps_coords_big_box(grid))
