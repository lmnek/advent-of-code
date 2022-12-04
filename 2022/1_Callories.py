# https://adventofcode.com/2022/day/1
import sys

def part1():
    max_callories = 0
    current_callories_sum = 0
    lines = sys.stdin.read().split('\n')
    for line in lines:
        if line == "":
            max_callories = max(max_callories, current_callories_sum)
            current_callories_sum = 0
        else:
            current_callories_sum += int(line)

    print(max_callories)

def part2():
    max_callories = [0, 0, 0]
    current_callories_sum = 0
    lines = sys.stdin.read().split('\n')
    for line in lines:
        if line == "":
            max_callories.sort()
            max_callories[0] = max(max_callories[0], current_callories_sum)
            current_callories_sum = 0
        else:
            current_callories_sum += int(line)

    max_callories.sort()
    max_callories[0] = max(max_callories[0], current_callories_sum)
    print(max_callories[0] + max_callories[1] + max_callories[2])

part2()