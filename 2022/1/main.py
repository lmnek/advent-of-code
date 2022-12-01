import sys

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