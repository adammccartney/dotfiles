#!/bin/sh
set -e
echo 'vm.swappiness=10' | tee -a /etc/sysctl.conf
