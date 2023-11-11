#!/usr/bin/python3
import argparse
import logging
import os
import subprocess as sproc

from pathlib import Path

__doc__ = """backup.py is a script to backup a filesystem

It's basically a wrapper around rsync and is aimed at backing up the $HOME
directory of personal laptops."""


def sharelogger():
    "Sets up a simple logger for use with the script"
    sharelogger = logging.getLogger('rsynchome.py')
    sharelogger.setLevel(logging.ERROR) # Use .DEBUG -> dev, .ERROR -> prod
    # console hanlder
    ch = logging.StreamHandler()
    # formatter
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    # add formatter to ch
    ch.setFormatter(formatter)
    # add ch to logger
    sharelogger.addHandler(ch)
    return sharelogger

logger = sharelogger() # Global logger for script


def getargs():
    parser = argparse.ArgumentParser(description=__doc__)
    user = os.environ["USER"]
    home = os.environ["HOME"]
    parser.add_argument("--old-home", required=False,
                        help="Root of old filesystem (source to copy)",
                        default=f"{home}")
    parser.add_argument("--new-home", required=False,
                        help="Root of new filesystem (destination for copy)",
                        default=f"/run/media/{user}/adb/backup/home/{user}")
    parser.add_argument("--excludes-file", required=True,
                        help="list of patterns to exclude from rsync")
    parser.add_argument("--check",
                        action=argparse.BooleanOptionalAction)
    return parser.parse_args()


def a_stat(file):
    "Get the uid and gid of the file fileectory"
    stat_info = os.stat(file)
    mode = stat_info.st_mode
    uid = stat_info.st_uid
    gid = stat_info.st_gid
    return mode, uid, gid

def mkdir_new_home(old_home: Path, new_home: str):
    """creates the path to destination where the home directory will be copied

    Makes the directory
    preserves mode, uid, gid from old home
    returns new home

    """
    mode, uid, gid = a_stat(old_home)
    result = Path(new_home)
    result.mkdir(mode=mode, parents=True, exist_ok=True)
    assert result.exists(), f"Error: failed to create {result}"
    os.chown(result, uid, gid)
    logger.info(f"{mkdir_new_home.__name__}: mkdir {result} mode: {mode}, uid: {uid}, gid: {gid}")
    return result


def c_rsync_home(src, dest, excludes_path, dry_run=False):
    """Custom rsync home

    Fork a subprocess to sync src to dest
    """
    if dry_run:
        cmd = f"rsync -ravP --dry-run --delete --cvs-exclude --exclude-from={excludes_path} '{src}/' '{dest}'"
    else:
        cmd = f"rsync -ravP --delete --cvs-exclude --exclude-from={excludes_path} '{src}/' '{dest}'"
    logger.info(f"{c_rsync_home.__name__}: {cmd}")
    try:
        sproc.run(cmd, shell=True)
    except Exception as e:
        logger.error(f"{c_rsync_home.__name__} {e}")


def main():
    os.chdir("/tmp")
    args = getargs()
    excludes_path = Path(args.excludes_file).resolve()
    src = Path(args.old_home).resolve()
    assert src.exists(), "Fatal error: old home does not exist"
    dest = mkdir_new_home(src, args.new_home)
    assert dest, "Error: dest does not exist!"
    try:
        if args.check:
            c_rsync_home(src, dest, excludes_path, dry_run=True)
        else:
            c_rsync_home(src, dest, excludes_path)
    except Exception as e:
        logger.error(f"main: {e}")

if __name__ == '__main__':
    main()
