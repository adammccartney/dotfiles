#!/bin/bash

# usage: source export_kubeconfigs.bash [K8S_DIR]
#        finds any config or kubeconfig files in K8S_DIR, exports them with
#        KUBECONFIG_ prefixes into the environment as variables
#        the "selkc" function can then be used to select between them

K8S_DIR=${1:-$HOME/infrastructure/k8s/clusters}
TMPFILE=$(mktemp)

# find all kubeconfig files matching this scheme
find "$K8S_DIR" -iname "*config" | xargs python3 -c '
import sys

from dataclasses import dataclass
from pathlib import Path

@dataclass
class Kubeconfig:
    path: str
    name: str

files = sys.argv[1:]
for file in files:
    p = Path(file)
    kc = Kubeconfig(str(p), p.parent.name)
    cmd = f"export KUBECONFIG_{kc.name.upper().replace("-", "_")}={kc.path}"
    print(cmd)
' > "$TMPFILE"

source "$TMPFILE"
rm "$TMPFILE"
