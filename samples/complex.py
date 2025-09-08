"""Complex Python example with nested scopes and closures."""

import os
from typing import List, Optional

CONFIG_PATH = "/etc/myapp.conf"
DEFAULT_TIMEOUT = 30


class DataProcessor:
    """Processes data with configurable filters."""

    def __init__(self, filters: List[str]):
        self.filters = filters
        self.processed_count = 0

    def process(self, data: List[str]) -> List[str]:
        """Process data through filters."""

        def apply_filter(item: str, filter_name: str) -> Optional[str]:
            """Inner function to apply a single filter."""
            if filter_name == "uppercase":
                return item.upper()
            elif filter_name == "lowercase":
                return item.lower()
            elif filter_name == "strip":
                return item.strip()
            return None

        def filter_chain(item: str) -> str:
            """Apply all filters in sequence."""
            result = item
            for filter_name in self.filters:
                filtered = apply_filter(result, filter_name)
                if filtered is not None:
                    result = filtered
            return result

        processed = []
        for item in data:
            transformed = filter_chain(item)
            processed.append(transformed)
            self.processed_count += 1

        return processed


def create_processor(config_file: str = CONFIG_PATH) -> DataProcessor:
    """Factory function to create processor from config."""

    def read_config(path: str) -> List[str]:
        """Read configuration from file."""
        if not os.path.exists(path):
            return ["strip", "lowercase"]

        # Simulated config reading
        return ["strip", "uppercase"]

    filters = read_config(config_file)
    return DataProcessor(filters)


def main():
    """Main function demonstrating nested scopes."""
    processor = create_processor()

    test_data = ["  Hello  ", "  WORLD  ", "  Python  "]
    result = processor.process(test_data)

    print(f"Processed {processor.processed_count} items")
    for item in result:
        print(f"-> {item}")


if __name__ == "__main__":
    main()
