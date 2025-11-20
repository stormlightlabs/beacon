# beacon: mode=strict
# This file tests strict mode type checking for implicit Any types in function signatures

# ERROR: Should trigger ANN007 - parameters have implicit Any types
def add(x, y):
    return x + y

# ERROR: Should trigger ANN008 - return type has implicit Any
def process_data(items):
    results = []
    for item in items:
        results.append(item * 2)
    return results

# ERROR: Both parameter and return type have implicit Any
def transform(data):
    return data.upper()

# OK: Properly annotated function
def multiply(x: int, y: int) -> int:
    return x * y

# OK: Function with None return type (procedure) doesn't need annotation
def print_message(msg: str) -> None:
    print(msg)

# ERROR: Parameter has implicit Any even though return type is annotated
def format_output(value) -> str:
    return str(value)

# ERROR: Return type has implicit Any even though parameters are annotated
def get_first(items: list):
    if items:
        return items[0]
    return None
