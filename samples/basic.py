#!/usr/bin/env python3
"""Basic Python examples for testing name resolution."""

# Global variables
PI = 3.14159
DEBUG = True


def greet(name, greeting="Hello"):
    """A simple greeting function."""
    message = f"{greeting}, {name}!"
    if DEBUG:
        print(f"Debug: greeting {name}")
    return message


class Calculator:
    """A basic calculator class."""

    def __init__(self, precision=2):
        self.precision = precision
        self.history = []

    def add(self, x, y):
        result = x + y
        self.history.append(f"{x} + {y} = {result}")
        return round(result, self.precision)

    def multiply(self, x, y):
        result = x * y
        self.history.append(f"{x} * {y} = {result}")
        return round(result, self.precision)


def main():
    """Runs test calculations and a greeting, then shows history"""
    calc = Calculator(precision=3)

    sum_result = calc.add(PI, 2.0)
    product = calc.multiply(sum_result, 2)

    user_name = "Alice"
    greeting_msg = greet(user_name)

    print(greeting_msg)
    print(f"Calculation result: {product}")

    for entry in calc.history:
        print(f"History: {entry}")


if __name__ == "__main__":
    main()
