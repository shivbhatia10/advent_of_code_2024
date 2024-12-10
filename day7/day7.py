def can_be_made_valid(arr: list[int], target: int, verbose: bool = True) -> bool:
    n = len(arr)

    if n == 1:
        return arr[0] == target

    def helper(index: int, curr: int, acc: str) -> bool:
        if index == n:
            if curr != target:
                return False
            if verbose:
                acc += " "
                for x, op in zip(arr, acc):
                    print(x, op, end=" ")
                print(f"= {target}")
            return True
        return helper(index + 1, curr * arr[index], acc + "*") or helper(
            index + 1, curr + arr[index], acc + "+"
        )

    return helper(1, arr[0], "")


def can_be_made_valid_with_concatenation(
    arr: list[int], target: int, verbose: bool = True
) -> bool:
    n = len(arr)

    if n == 1:
        return arr[0] == target

    def helper(index: int, curr: int, acc: str) -> bool:
        if index == n:
            if curr != target:
                return False
            if verbose:
                acc += " "
                for x, op in zip(arr, acc):
                    print(x, op, end=" ")
                print(f"= {target}")
            return True
        return (
            helper(index + 1, curr * arr[index], acc + "*")
            or helper(index + 1, curr + arr[index], acc + "+")
            or helper(index + 1, int(str(curr) + str(arr[index])), acc + "|")
        )

    return helper(1, arr[0], "")


with open("day7_input.txt") as f:
    lines = f.readlines()
    res = 0
    res2 = 0
    for line in lines:
        [target, arr] = line.split(": ")
        if can_be_made_valid(list(map(int, arr.split())), int(target), verbose=False):
            res += int(target)
        if can_be_made_valid_with_concatenation(
            list(map(int, arr.split())), int(target)
        ):
            res2 += int(target)
    # print(res)
    print(res2)
