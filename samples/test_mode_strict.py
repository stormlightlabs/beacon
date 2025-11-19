# beacon: mode=strict
# This file uses strict mode type checking

def add(x, y):
    return x + y

# This should trigger errors in strict mode due to missing type annotations
result = add(1, 2)
print(result)
