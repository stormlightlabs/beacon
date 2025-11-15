"""Test cases for recursive type handling."""

from typing import Optional, Union

# Recursive class - this should work
class LinkedListNode:
    def __init__(self, value: int, next: Optional['LinkedListNode'] = None):
        self.value = value
        self.next = next

def create_list(x: int) -> LinkedListNode:
    return LinkedListNode(x, None)

def append_node(head: LinkedListNode, value: int) -> LinkedListNode:
    return LinkedListNode(value, head)

# Tree structure with recursion
class TreeNode:
    def __init__(self, value: int, left: Optional['TreeNode'] = None, right: Optional['TreeNode'] = None):
        self.value = value
        self.left = left
        self.right = right

def create_tree(val: int) -> TreeNode:
    return TreeNode(val)

def add_left_child(parent: TreeNode, value: int) -> TreeNode:
    return TreeNode(value, parent, None)
