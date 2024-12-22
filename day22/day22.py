from collections import defaultdict


def simulate(secret_num: int) -> int:
    step1 = ((64 * secret_num) ^ secret_num) % 16777216
    step2 = ((step1 // 32) ^ step1) % 16777216
    step3 = ((step2 * 2048) ^ step2) % 16777216
    return step3


def simulate_n_times(secret_num: int, n: int) -> int:
    for _ in range(n):
        secret_num = simulate(secret_num)
    return secret_num


def generate_sequence(secret_num: int, n: int) -> list[int]:
    sequence = [secret_num]
    for _ in range(n):
        sequence.append(simulate(sequence[-1]))
    return sequence


filename = "day22_input.txt"
with open(filename) as f:
    nums = [int(line.strip()) for line in f.read().split("\n")]
# nums = [1, 2, 3, 2024]

# res = 0
# for num in nums:
#     print(num)
#     res += simulate_n_times(num, 2000)

# print("Result:", res)

sequences = [generate_sequence(x, 2000) for x in nums]
prices = [[x % 10 for x in seq] for seq in sequences]
diffs = [[b - a for a, b in zip(seq, seq[1:])] for seq in prices]
print(len(diffs[0]))

d: dict[tuple[int, int, int, int], dict[int, int]] = defaultdict(
    lambda: defaultdict(int)
)
for buyer, buyer_diffs in enumerate(diffs):
    for i in range(3, len(buyer_diffs)):
        diff_tuple = (
            buyer_diffs[i - 3],
            buyer_diffs[i - 2],
            buyer_diffs[i - 1],
            buyer_diffs[i],
        )
        if buyer in d[diff_tuple]:
            continue
        d[diff_tuple][buyer] = max(
            d[diff_tuple][buyer],
            prices[buyer][i + 1],
        )

best_seq = max(d.keys(), key=lambda k: sum(d[k].values()))
print(best_seq)

best_profit = sum(d[best_seq].values())
print(best_profit)
