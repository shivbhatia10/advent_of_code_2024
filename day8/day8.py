from collections import defaultdict

with open("day8_input.txt") as f:
    data = [list(line) for line in f.read().split("\n")]
    n, m = len(data), len(data[0])
    freq_to_positions = defaultdict(list)
    for i in range(n):
        for j in range(m):
            if data[i][j] == ".":
                continue
            freq_to_positions[data[i][j]].append((i, j))

    def get_signal_impact(positions: list[tuple[int, int]]) -> set[tuple[int, int]]:
        res = set()
        for first_index, (x1, y1) in enumerate(positions):
            for x2, y2 in positions[first_index + 1 :]:
                dx, dy = x2 - x1, y2 - y1
                if x1 - dx in range(n) and y1 - dy in range(m):
                    res.add((x1 - dx, y1 - dy))
                if x2 + dx in range(n) and y2 + dy in range(m):
                    res.add((x2 + dx, y2 + dy))
        return res

    def get_signal_impact_with_resonance(
        positions: list[tuple[int, int]]
    ) -> set[tuple[int, int]]:
        res = set()
        for first_index, (x1, y1) in enumerate(positions):
            for x2, y2 in positions[first_index + 1 :]:
                dx, dy = x2 - x1, y2 - y1
                curr_x, curr_y = (x1, y1)
                while curr_x in range(n) and curr_y in range(m):
                    res.add((curr_x, curr_y))
                    curr_x, curr_y = curr_x - dx, curr_y - dy
                curr_x, curr_y = (x2, y2)
                while curr_x in range(n) and curr_y in range(m):
                    res.add((curr_x, curr_y))
                    curr_x, curr_y = curr_x + dx, curr_y + dy
        return res

    # Part 1
    total_impact = set()
    for freq in freq_to_positions:
        positions = freq_to_positions[freq]
        impact = get_signal_impact(positions)
        total_impact |= impact
    print(len(total_impact))

    # Part 2
    total_impact = set()
    for freq in freq_to_positions:
        positions = freq_to_positions[freq]
        impact = get_signal_impact_with_resonance(positions)
        total_impact |= impact
    print(len(total_impact))
