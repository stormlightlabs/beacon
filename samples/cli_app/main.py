import argparse
from .utils import helper_function, missing_function

def main():
    parser = argparse.ArgumentParser(description="Sample CLI App")
    parser.add_argument("--name", help="Name to greet")
    args = parser.parse_args()

    if args.name:
        print(helper_function(args.name))
        print(missing_function()) # This should trigger an error
    else:
        print("Hello, World!")

if __name__ == "__main__":
    main()
