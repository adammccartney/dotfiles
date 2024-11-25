#!/usr/bin/env python3
import argparse
import os

from pathlib import Path

__doc__ = "Setup a basic directory structure for ansible"

def role_mkdirs(basedir: Path, roles: list[str]):
    "Return the role directory tree"
    dirs = []
    if not roles:
        return
    roles_node = basedir.joinpath("roles")
    for r in roles:
        role_node = roles_node.joinpath(r)
        dirs.append(role_node)
        for n in ["files", "tasks", "templates", "vars"]:
            node = role_node.joinpath(n)
            dirs.append(node)
    for d in dirs:
        os.makedirs(d)
    return dirs

def touch_playbooks(basedir, playbooks):
    "Create playbook files"
    for p in playbooks:
        Path(basedir).joinpath(p).touch()

def touch_cfg(basedir):
    Path(basedir).joinpath("ansible.cfg").touch()


def write_config(basedir):
    cfg = basedir.joinpath("ansible.cfg")
    cfg.touch()
    default_lines = ["[defaults]\n", "inventory = inventory"]
    with open(cfg, "w") as f:
        f.writelines(default_lines)


def mkdir_inventory(basedir):
    def touch_hosts(inventory: Path):
        inventory.joinpath("hosts").touch() 
    inv = Path(basedir).joinpath("inventory")
    inv.mkdir()
    touch_hosts(inv)

def getargs():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", "-c", action='store',
                        default="ansible.cfg",
                        help="Ansible config file")
    parser.add_argument("--base", "-b", action='store',
                          help='Add the base directory for playbooks')
    parser.add_argument("--role", "-r", 
                        action='append', 
                        help="Add a role for cofiguration")
    parser.add_argument("--playbook", "-p", action='append',
                        help="Add a playbook at the top level") 
    return parser.parse_args()


def main():
    args = getargs()
    basedir = Path(args.base)
    write_config(basedir)
    roles = args.role
    role_mkdirs(basedir, roles) 
    playbooks = [p for p in args.playbook]
    touch_playbooks(basedir, playbooks)
    mkdir_inventory(basedir)

if __name__ == "__main__":
    main()
