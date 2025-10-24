import argparse

def getargs():
    "Snarf up whatever's on the command line"
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--your-arg-here", "-y", default=None,
                        help="This is the format of an arg")
    return parser.parse_args()

def main():
    args = getargs()

if __name__ == '__main__':
    main()
