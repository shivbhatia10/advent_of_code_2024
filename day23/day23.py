from collections import defaultdict


filename = "day23_input.txt"

adj: dict[str, set] = defaultdict(set)
with open(filename) as file:
    for line in file.read().split("\n"):
        [a, b] = line.split("-")
        adj[a].add(b)
        adj[b].add(a)

triplets = set()
for a in adj.keys():
    neis = list(adj[a])
    for i in range(len(neis)):
        for j in range(i + 1, len(neis)):
            if neis[i] in adj[neis[j]]:
                triplets.add(tuple(sorted([a, neis[i], neis[j]])))

t_triplets = 0
for triplet in triplets:
    (a, b, c) = triplet
    if a[0] == "t" or b[0] == "t" or c[0] == "t":
        t_triplets += 1

print(t_triplets)

computers = list(adj.keys())
parties = [{x} for x in computers]
print(parties)

changed = True
while changed:
    changed = False
    for i in range(len(computers)):
        for j in range(len(parties)):
            if computers[i] not in parties[j] and all(
                party_member in adj[computers[i]] for party_member in parties[j]
            ):
                print("BOO")
                parties[j].add(computers[i])
                changed = True

print(parties)


def check_party(party: set[str]) -> bool:
    for x in party:
        for y in party:
            if x != y and y not in adj[x]:
                return False
    return True


for party in parties:
    if not check_party(party):
        print("NO")

largest_party = max(parties, key=len)
print(largest_party)

print(",".join(sorted(largest_party)))
