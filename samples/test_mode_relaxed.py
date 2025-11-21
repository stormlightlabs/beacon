# beacon: mode=relaxed
# This file uses relaxed mode type checking

def add(x, y):
    return x + y

# This should not trigger errors in relaxed mode
result = add(1, 2)
print(result)
