"""Demonstration of core Python language features.

Use this to test parsing.
"""


def controls(n: int) -> list[int]:
    """Demo of control flow statements: if, for, while, and try/except.

    Args:
        n (int): A positive integer to iterate up to.

    Returns:
        list[int]: A list of integers computed from control flow operations.
    """
    results = []
    try:
        for i in range(n):
            if i % 2 == 0:
                results.append(i)
            else:
                results.append(i * 2)
        count = 0
        while count < 3:
            results.append(count + n)
            count += 1
    except Exception as e:
        print(f"An error occurred: {e}")
    return results


def with_stmts(filepath: str) -> str:
    """Demonstrate the use of the 'with' statement for resource management.

    The 'with' statement ensures that files or other resources are automatically
    closed or released after use, even if an error occurs.

    Args:
        filepath (str): The path to a file.

    Returns:
        str: The file's contents.
    """
    with open(filepath, "r", encoding="utf-8") as file:
        contents = file.read()
    return contents


def comps(
    n: int,
) -> tuple[list[int], dict[int, int], set[int], list[int]]:
    """Demonstrate Python comprehensions: list, dict, set, and generator.

    Args:
        n (int): The range limit for the comprehensions.

    Returns:
        tuple: A tuple containing examples of each comprehension type.
    """
    list_comp = [i**2 for i in range(n)]
    dict_comp = {i: i**2 for i in range(n)}
    set_comp = {i % 3 for i in range(n)}
    gen_comp = (i**3 for i in range(n))  # generator expression
    return list_comp, dict_comp, set_comp, list(gen_comp)


def walrus(threshold: int) -> list[int]:
    """Demo the 'walrus operator' (:=), introduced in Python 3.8.

    The walrus operator allows assignment within expressions.
    Useful for reducing redundancy when checking or computing values inline.

    Args:
        threshold (int): The cutoff value for filtering.

    Returns:
        list[int]: A list of numbers below the threshold that meet a condition.
    """
    nums = [5, 10, 15, 20, 25]

    filtered = [n for n in nums if (half := n) and half / 2 < threshold]
    return filtered


def patterns(value: object) -> str:
    """Demo Python 3.10's structural pattern matching (match/case).

    Pattern matching provides a more expressive and readable alternative
    to chains of if/elif statements.

    Args:
        value (object): Any value to match against patterns.

    Returns:
        str: A message describing the matched pattern.
    """
    match value:
        case int() as n if n > 0:
            return f"Positive integer: {n}"
        case int() as n if n < 0:
            return f"Negative integer: {n}"
        case str() as s:
            return f"String of length {len(s)}"
        case [x, y]:
            return f"List of two elements: {x}, {y}"
        case _:
            return "Unknown pattern"


def op_expr(a: int, b: int) -> dict[str, int | bool]:
    """Demo operator expressions: binary, unary, and comparison operators.

    Args:
        a (int): First operand.
        b (int): Second operand.

    Returns:
        dict[str, int | bool]: Results of various operator expressions.
    """
    results = {
        "addition": a + b,
        "subtraction": a - b,
        "multiplication": a * b,
        "division": a / b if b != 0 else None,
        "negation": -a,
        "comparison_eq": a == b,
        "comparison_gt": a > b,
        "comparison_lt": a < b,
    }
    return results
