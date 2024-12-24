from __future__ import annotations
from enum import Enum
from collections import deque
from typing import Callable


class NodeType(Enum):
    LEAF = 0
    AND = 1
    OR = 2
    XOR = 3


class Node:
    def __init__(
        self,
        name: str,
        val: int | None,
        type: NodeType,
        left: Node | None,
        right: Node | None,
    ) -> None:
        self.name = name
        self.val = val
        self.type = type
        self.left = left
        self.right = right

    def eval(self) -> int:
        if self.val is not None:
            return self.val

        if self.type == NodeType.AND:
            if self.right is None or self.left is None:
                raise ValueError("AND node has no left or right node")
            res = self.left.eval() & self.right.eval()
        elif self.type == NodeType.OR:
            if self.right is None or self.left is None:
                raise ValueError("AND node has no left or right node")
            res = self.left.eval() | self.right.eval()
        elif self.type == NodeType.XOR:
            if self.right is None or self.left is None:
                raise ValueError("AND node has no left or right node")
            res = self.left.eval() ^ self.right.eval()
        else:
            raise ValueError("Invalid NodeType")
        self.val = res
        return res

    def __repr__(self):
        if self.type == NodeType.LEAF:
            return f"{self.name}({self.val})"
        return f"{self.name}({self.type})"


def construct_tree(file_name: str, swaps: dict[str, str] = {}) -> dict[str, Node]:
    name_to_node: dict[str, Node] = {}
    with open(file_name) as f:
        initial, trees = f.read().strip().split("\n\n")
        for line in initial.split("\n"):
            name, val = line.split(": ")
            name_to_node[name] = Node(name, int(val), NodeType.LEAF, None, None)

        # First pass to get all the nodes
        for line in trees.split("\n"):
            left, _, right, _, node = line.split(" ")
            if node not in name_to_node:
                name_to_node[node] = Node(node, None, NodeType.LEAF, None, None)
            if left not in name_to_node:
                name_to_node[left] = Node(left, None, NodeType.LEAF, None, None)
            if right not in name_to_node:
                name_to_node[right] = Node(right, None, NodeType.LEAF, None, None)

        # Second pass to link the nodes
        for line in trees.split("\n"):
            pieces = line.split(" ")
            left, op, right, _, node = pieces
            if node in swaps:
                node = swaps[node]
            if op == "AND":
                name_to_node[node].left = name_to_node[left]
                name_to_node[node].right = name_to_node[right]
                name_to_node[node].type = NodeType.AND
            elif op == "OR":
                name_to_node[node].left = name_to_node[left]
                name_to_node[node].right = name_to_node[right]
                name_to_node[node].type = NodeType.OR
            elif op == "XOR":
                name_to_node[node].left = name_to_node[left]
                name_to_node[node].right = name_to_node[right]
                name_to_node[node].type = NodeType.XOR
    return name_to_node


def get_digit_names(tree: dict[str, Node], starts_with: str) -> list[str]:
    return sorted(name for name in tree if name.startswith(starts_with))


def read_register_from_tree(tree: dict[str, Node], starts_with: str) -> int:
    digit_names = get_digit_names(tree, starts_with)
    digits = [name_to_node[name].eval() for name in digit_names]
    return sum(digit << i for i, digit in enumerate(digits))


def print_registers(tree: dict[str, Node]) -> None:
    x_value = read_register_from_tree(tree, "x")
    y_value = read_register_from_tree(tree, "y")
    z_value = read_register_from_tree(tree, "z")
    print(
        f"X: {bin(x_value)}\nY: {bin(y_value)}\nZ:   {bin(z_value)}\nX+Y: {bin(x_value + y_value)}"
    )


def print_levels_from_root(root: Node, max_depth: int | float = float("inf")) -> None:
    print()
    queue = deque([(root, 0)])
    while queue:
        node, depth = queue.popleft()
        if node.type == NodeType.LEAF:
            print(node)
        else:
            print(f"{node} -> {node.left} {node.right}")
            if depth < max_depth and node.left and node.right:
                queue.append((node.left, depth + 1))
                queue.append((node.right, depth + 1))
    print()


def matches_output_bit_pattern(node: Node) -> bool:
    return (
        node.type == NodeType.XOR
        and node.left is not None
        and node.right is not None
        and (node.left.type == NodeType.XOR or node.right.type == NodeType.XOR)
    )


def matches_carry_bit_pattern(node: Node) -> bool:
    return (
        node.type == NodeType.OR
        and node.left is not None
        and node.right is not None
        and node.left.type == NodeType.AND
        and node.right.type == NodeType.AND
    )


def matches_intermediate_pattern_1(node: Node) -> bool:
    return (
        node.type == NodeType.XOR
        and node.left is not None
        and node.right is not None
        and node.left.type == NodeType.LEAF
        and node.right.type == NodeType.LEAF
    )


def matches_intermediate_pattern_2(node: Node) -> bool:
    return (
        node.type == NodeType.AND
        and node.left is not None
        and node.right is not None
        and node.left.type == NodeType.LEAF
        and node.right.type == NodeType.LEAF
    )


def matches_intermediate_pattern_3(node: Node) -> bool:
    return (
        node.type == NodeType.AND
        and node.left is not None
        and node.right is not None
        and ((node.left.type == NodeType.XOR) or (node.right.type == NodeType.XOR))
    )


def matches_leaf_pattern(node: Node) -> bool:
    return node.type == NodeType.LEAF


def matches_any_pattern(node: Node) -> bool:
    return (
        matches_output_bit_pattern(node)
        or matches_carry_bit_pattern(node)
        or matches_intermediate_pattern_1(node)
        or matches_intermediate_pattern_2(node)
        or matches_intermediate_pattern_3(node)
        or matches_leaf_pattern(node)
    )


def find_or_throw(
    tree: dict[str, Node],
    pattern: Callable[[Node], bool],
    left_name: str,
    right_name: str,
) -> str:
    valid_names = []
    for name in tree.keys():
        node = tree[name]
        left, right = node.left, node.right
        if (
            pattern(node)
            and left is not None
            and right is not None
            and (left.name == left_name or left.name == right_name)
            and (right.name == right_name or right.name == left_name)
        ):
            valid_names.append(name)
    if len(valid_names) != 1:
        raise ValueError(
            f"Expected to find one node meeting pattern {pattern} with left name {left_name} and right name {right_name}, found {len(valid_names)}"
        )
    return valid_names[0]


def get_all_nodes_with_child(tree: dict[str, Node], child: str) -> list[str]:
    res = []
    for name in tree.keys():
        node = tree[name]
        if node.left and node.left.name == child:
            res.append(name)
        if node.right and node.right.name == child:
            res.append(name)
    return res


filename = "day24_input.txt"
name_to_node = construct_tree(
    filename,
    swaps={
        "qjj": "gjc",
        "gjc": "qjj",
        "wmp": "z17",
        "z17": "wmp",
        "z26": "gvm",
        "gvm": "z26",
        "z39": "qsb",
        "qsb": "z39",
    },
)


print_registers(name_to_node)
c_in = "wbd"
for adder in range(1, 45):
    print()
    print(f"Looking in adder number {adder}")
    x, y = f"x{adder:02}", f"y{adder:02}"

    # Intermediate_pattern_1
    print(f"Intermediate_pattern_1: {x} + {y}")
    i1 = find_or_throw(name_to_node, matches_intermediate_pattern_1, x, y)
    print(f"i1: {i1}")

    # Intermediate_pattern_2
    print(f"Intermediate_pattern_2: {x} + {y}")
    i2 = find_or_throw(name_to_node, matches_intermediate_pattern_2, x, y)
    print(f"i2: {i2}")

    # Intermediate_pattern_3
    print(f"Intermediate_pattern_3: {i1} + {c_in}")
    i3 = find_or_throw(name_to_node, matches_intermediate_pattern_3, i1, c_in)
    print(f"i3: {i3}")

    # Output_bit_pattern
    print(f"Output_bit_pattern: {i1} + {c_in}")
    z = find_or_throw(name_to_node, matches_output_bit_pattern, i1, c_in)
    print(f"z: {z}")

    # Carry_bit_pattern (becomes c_in for next iteration)
    print(f"Carry_bit_pattern: {i3} + {i2}")
    c_in = find_or_throw(name_to_node, matches_carry_bit_pattern, i3, i2)
    print(f"c_in: {c_in}")

ans = ["qjj", "gjc", "wmp", "z17", "z26", "gvm", "z39", "qsb"]
print("At long last, the answer is:")
print(",".join(sorted(ans)))
