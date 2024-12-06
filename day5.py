from collections import defaultdict
from collections.abc import Generator

with open("day5_input.txt") as f:
    [section1, section2] = f.read().split("\n\n")
    rules = section1.split("\n")
    before: dict[str, set[str]] = defaultdict(set)
    after: dict[str, set[str]] = defaultdict(set)
    for rule in rules:
        a, b = rule.split("|")
        before[b.strip()].add(a.strip())
        after[a.strip()].add(b.strip())

    updates = section2.split("\n")

    # Part 1
    res = 0
    for update in updates:
        pages = update.split(",")
        disallowed: set[str] = set()
        is_allowed = True
        for page in pages:
            if page in disallowed:
                is_allowed = False
                break
            for before_page in before[page.strip()]:
                disallowed.add(before_page)
        if is_allowed:
            res += int(pages[len(pages) // 2])
    print(res)

    # Part 2
    def get_root(pages: set[str]) -> str:
        # Quite proud of this line, very slow though
        return [
            page
            for page in pages
            if not any(other_page in before[page] for other_page in pages)
        ][0]

    def get_correct_ordering(pages: list[str]) -> Generator[str]:
        page_set = set(pages)
        for _ in range(len(pages)):
            root = get_root(page_set)
            page_set.remove(root)
            yield root

    res = 0
    for update in updates:
        pages = update.split(",")
        disallowed: set[str] = set()
        is_allowed = True
        for page in pages:
            if page in disallowed:
                is_allowed = False
                break
            for before_page in before[page.strip()]:
                disallowed.add(before_page)

        if not is_allowed:
            res += int(list(get_correct_ordering(pages))[len(pages) // 2])
    print(res)
