from collections import defaultdict
import tqdm

guard_symbols_to_direction = {"^": (-1, 0), "v": (1, 0), "<": (0, -1), ">": (0, 1)}


def reverse(direction: tuple[int, int]) -> tuple[int, int]:
    return (-direction[0], -direction[1])


def rotate_right(direction: tuple[int, int]) -> tuple[int, int]:
    return (direction[1], -direction[0])


def rotate_left(direction: tuple[int, int]) -> tuple[int, int]:
    return rotate_right(reverse(direction))


with open("day6_input.txt") as f:
    grid = [list(line) for line in f.read().split("\n")]
    n, m = len(grid), len(grid[0])

    def is_within_bounds(pos):
        return pos[0] in range(n) and pos[1] in range(m)

    def get_guard_pos_and_direction() -> tuple[tuple[int, int], tuple[int, int]]:
        for i in range(n):
            for j in range(m):
                if grid[i][j] in guard_symbols_to_direction:
                    return (i, j), guard_symbols_to_direction[grid[i][j]]
        # Guard not found, should not happen
        return (0, 0), (0, 0)

    def count_guard_positions(pos: tuple[int, int], direction: tuple[int, int]) -> int:
        """
        Count the number of positions the guard visits before exiting the board.
        Returns -1 if the guard is stuck in a loop.
        """
        visited = defaultdict(list)
        is_on_board = True
        while is_on_board:
            visited[pos].append(direction)
            next_pos = (
                pos[0] + direction[0],
                pos[1] + direction[1],
            )
            if is_within_bounds(next_pos):
                if grid[next_pos[0]][next_pos[1]] == "#":
                    direction = rotate_right(direction)
                else:
                    pos = next_pos

                if direction in visited[pos]:
                    return -1
            else:
                is_on_board = False
        return len(visited)

    # Part 1
    pos, direction = get_guard_pos_and_direction()
    num_guard_positions = count_guard_positions(pos, direction)
    print(num_guard_positions)

    # Part 2
    pos, direction = get_guard_pos_and_direction()
    loop_obstacles = set()
    empty_cells = [(i, j) for i in range(n) for j in range(m) if grid[i][j] == "."]
    for i, j in tqdm.tqdm(empty_cells):
        grid[i][j] = "#"
        num_guard_positions = count_guard_positions(pos, direction)
        if num_guard_positions == -1:
            loop_obstacles.add((i, j))
        grid[i][j] = "."
    print(len(loop_obstacles))
