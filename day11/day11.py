# I did this one in a very convoluted way, using a linked list of stones that would blink and split in two if they had an even number of digits.
# Then I realised it was a simple DP problem and solved it in a much simpler and faster way. I kept the linked list code here for posterity.

# from __future__ import annotations

from functools import cache

# import tqdm


# class Stone:
#     def __init__(
#         self, val: int, prev: Stone | None = None, next: Stone | None = None
#     ) -> None:
#         self.val = val
#         self.prev = prev
#         self.next = next

#     def has_even_digits(self) -> bool:
#         return len(str(self.val)) % 2 == 0

#     def blink(self) -> bool:
#         """
#         Returns True if a new stone was inserted after this one, False otherwise.
#         """
#         if self.val == 0:
#             self.val = 1
#             return False
#         elif self.has_even_digits():
#             a, b = self.get_split_values()
#             self.val = a
#             self.insert_after(Stone(b))
#             return True
#         self.val *= 2024
#         return False

#     def get_split_values(self) -> tuple[int, int]:
#         assert self.has_even_digits()
#         str_val = str(self.val)
#         a, b = str_val[: len(str_val) // 2], str_val[len(str_val) // 2 :]
#         return int(a), int(b)

#     def insert_after(self, other: Stone) -> None:
#         other.next = self.next
#         other.prev = self
#         if self.next is not None:
#             self.next.prev = other
#         self.next = other


# head, tail = Stone(0), Stone(0)
# curr = head

vals: list[int] = []
with open("day11_input.txt") as f:
    for val in map(int, f.read().split()):
        # curr.insert_after(Stone(val))
        # if curr.next is not None:
        #     curr = curr.next

        vals.append(val)

# curr.next = tail


# Part 1
# def print_stones():
#     curr = head.next
#     while curr != tail:
#         print(curr.val, end=" ")
#         curr = curr.next
#     print()


BLINKS = 25

# for _ in tqdm.tqdm(range(BLINKS)):
#     if head.next is None:
#         raise ValueError("First stone is null")
#     curr = head.next
#     while curr != tail:
#         new_stone_inserted = curr.blink()
#         if new_stone_inserted:
#             assert curr.next is not None
#             curr = curr.next
#         assert curr.next is not None
#         curr = curr.next

# total = 0
# assert head.next is not None
# curr = head.next
# while curr != tail:
#     total += 1
#     assert curr.next is not None
#     curr = curr.next

# print(total)


# Part 2
BLINKS2 = 75


@cache
def count_stones(val, blinks_remaining) -> int:
    if blinks_remaining == 0:
        return 1
    if val == 0:
        return count_stones(1, blinks_remaining - 1)
    if len(str(val)) % 2 == 0:
        str_val = str(val)
        a, b = str_val[: len(str_val) // 2], str_val[len(str_val) // 2 :]
        return count_stones(int(a), blinks_remaining - 1) + count_stones(
            int(b), blinks_remaining - 1
        )
    return count_stones(val * 2024, blinks_remaining - 1)


total1 = sum(count_stones(val, BLINKS) for val in vals)
total2 = sum(count_stones(val, BLINKS2) for val in vals)
print(total1)
print(total2)
