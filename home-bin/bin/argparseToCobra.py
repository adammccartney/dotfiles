import argparse
import sys
import logging
import importlib

__doc__ = """Convert argparse arguments to cobra"""

logger = logging.getLogger(__name__)

parser = argparse.ArgumentParser()
parser.add_argument(
        "--writeable-repository",
        "-w",
        default="software.asc.ac.at",
        help="CVMFS repository mouned with writeable overlay filesystem"
)
#parser.add_argument(
#        "--container-image",
#        "-c",
#        default=global_env["default_ctr_img"],
#        help="Container image to use at runtime",
#)
parser.add_argument(
        "--resume",
        "-r",
        default=None,
        help="Resume path for the container",
)
parser.add_argument(
        "--extra-bind-paths",
        "-b",
        default=None,
        help="Extra bind mounts to pass to apptainer",
)
parser.add_argument(
        "--nvidia",
        "-n",
        default="all",
        help="pass extra container arg to apptainer to setup nvidia gpu"
)
parser.add_argument(
        "--host-injections",
        "-i",
        default="/opt/eessi",
        help="""Host injections path for EESSI. Non-default paths are often
        defined in /etc/cvmfs/domain.d/eessi.io.local"""
)
parser.add_argument(
        "--to-stdout",
        action="store_true",
        help="Do not run final command. Print to stdout instead."
)

def fmt_go_cobra(parser: argparse.ArgumentParser):
    pass


def main():
    usage = f"{__file__} module.py"
    try:
        if len(sys.argv) != 2:
            raise ValueError(f"usage: {usage}")
        else:
            mf = sys.argv[1]
            mname = mf.strip(".py")
            module = importlib.import_module(f".{mname}")
    except Exception as e:
        logger.error(f"Fatal {e}")
        sys.exit(1)



if __name__ == '__main__':
    main()
    #args = parser.parse_args()
    #for i in args._get_kwargs():
    #    k, v = i[0], i[1]
    #    print(f"{k}: {v}")
