# beacon: mode=loose
# This file uses loose mode type checking

def add(x, y):
    return x + y

# This should not trigger errors in loose mode
result = add(1, 2)
print(result)
