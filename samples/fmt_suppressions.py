"""Examples of formatter suppression directives"""

# Example 1: fmt: skip - Skip formatting for a single line
x = 1
y=2+3  # fmt: skip
z = 4

# Example 2: fmt: off/on - Disable formatting for a region
formatted_dict = {"key": "value", "another": "pair"}

# fmt: off
unformatted_dict={"key":"value","no":"spaces"}
complex_expression=1+2+3+4+5+6+7+8+9
# fmt: on

back_to_formatted = {"key": "value"}

# Example 3: Preserving intentional formatting
matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9],
]

# fmt: off
aligned_matrix = [
    [1,    2,    3],
    [100,  200,  300],
    [10,   20,   30],
]
# fmt: on

# Example 4: Mixed suppressions
def example_function():
    normal_var = 123
    special_var=456  # fmt: skip

    # fmt: off
    data={
        "compact":"format",
        "no":"spaces"
    }
    # fmt: on

    return {"formatted": "normally"}

# Example 5: Comment-only lines in fmt: off regions
# fmt: off

# This comment is in the suppressed region
unformatted=True

# fmt: on

# Example 6: Nested structures
outer = {
    "formatted": "yes",
    # fmt: off
    "inner":{"unformatted":"dict"},
    # fmt: on
    "back": "to normal",
}
