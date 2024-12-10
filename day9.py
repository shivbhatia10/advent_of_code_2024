def checksum(file: list[int]) -> int:
    """
    Returns the checksum for a compressed file
    """
    return sum(i * x for i, x in enumerate(file))


class DLLNode:
    def __init__(
        self,
        idx: int | None,
        count: int,
        next=None,
        prev=None,
    ):
        self.idx = idx
        self.count = count
        self.next = next
        self.prev = prev

    def insert_after(self, node):
        node.next = self.next
        node.prev = self
        self.next.prev = node
        self.next = node

    def delete(self):
        if self.prev is not None:
            self.prev.next = self.next
        if self.next is not None:
            self.next.prev = self.prev

    def print_in_order(self):
        curr = self
        while curr is not None:
            print(curr.idx, curr.count)
            curr = curr.next


def compactify(source_file: list[int | None]) -> list[int | None]:
    """
    Returns the compactified file
    """
    file = source_file[:]
    l, r = 0, len(file) - 1
    while l < r:
        while file[l] is not None:
            l += 1
        while file[r] is None:
            r -= 1
        if l < r:
            file[l], file[r] = file[r], file[l]
    return file


with open("day9_input.txt") as f:
    data = [int(x) for x in f.read().strip()]
    file: list[int | None] = []

    # Part 1
    curr_idx = 0
    is_empty = False
    for x in data:
        if is_empty:
            for _ in range(x):
                file.append(None)
        else:
            for _ in range(x):
                file.append(curr_idx)
            curr_idx += 1
        is_empty = not is_empty

    compact_file = compactify(file)
    print(checksum([x for x in compact_file if x is not None]))

    # Part 2
    file_head = DLLNode(None, 0)
    curr_node = file_head
    curr_idx = 0
    is_empty = False
    for x in data:
        new_node = DLLNode(None if is_empty else curr_idx, x)
        curr_node.next = new_node
        new_node.prev = curr_node
        curr_node = new_node
        if not is_empty:
            curr_idx += 1
        is_empty = not is_empty
    file_tail = DLLNode(None, 0)
    curr_node.next = file_tail
    file_tail.prev = curr_node

    l, r = file_head.next, file_tail.prev
    while file_head != r:
        if l == r:
            r = r.prev
            l = file_head.next
            continue
        if r.idx is None:
            r = r.prev
            continue
        if l.idx is not None or l.count < r.count:
            l = l.next
            continue
        l.prev.insert_after(DLLNode(r.idx, r.count))
        l.count -= r.count
        l = file_head.next
        if r.prev.idx is None:
            r.count += r.prev.count
            r.prev.delete()
        if r.next.idx is None:
            r.count += r.next.count
            r.next.delete()
        r.idx = None
        r = r.prev

    curr = file_head.next
    file2 = []
    while curr is not None:
        for _ in range(curr.count):
            file2.append(curr.idx if curr.idx is not None else 0)
        curr = curr.next
    print(checksum(file2))
