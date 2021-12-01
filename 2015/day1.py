def get_lines(file):
    with open(file, 'r') as f:
        return f.read().splitlines()
        
def part1():
    input = get_lines("input1.txt")[0]
    floor = 0

    for c in input:
        floor = floor + 1 if c == '(' else floor - 1

    return floor

def part2():
    input = get_lines("input1.txt")[0]
    floor = 0

    for i, c in enumerate(input):
        floor = floor + 1 if c == '(' else floor - 1
        if floor == -1:
            return i+1
