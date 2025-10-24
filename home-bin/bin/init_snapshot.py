#!/usr/bin/env python
import datetime
import shlex
import subprocess as sp
import time

import logging

logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s - %(levelname)s - %(message)s",
        datefmt="%Y-%m-%dT%H:%M:%S",
    )
logger = logging.getLogger(__name__)


def run_cmd(cmd):
    args = shlex.split(cmd)
    try:
        proc = sp.run(args, capture_output=True)
        return proc.returncode
    except Exception as err:
        logger.error(err)
        return 1 # return failure

def main():
    init_cmd = "sudo /usr/bin/cvmfs_server snapshot software.eessi.io"
    while True:
        rc = run_cmd(init_cmd)
        if rc == 0: # success
            logger.info("completed")
            break

        logger.info(f"restarting {init_cmd}")
        time.sleep(5)

if __name__ == '__main__':
    main()
