#!/usr/bin/sh
#
# wrapper for home backups

if command ${HOME}/bin/backup.py -h > /dev/null; then
    LOGFILE=/var/log/backup-$(date --iso-8601).log
    sudo touch ${LOGFILE} && sudo chown amccartn:amccartn ${LOGFILE}
    ${HOME}/bin/backup.py --old-home=${HOME} --new-home=/run/media/${USER}/adb/backup/home/${USER} --excludes-file=${HOME}/bin/bk-excludes.txt 2> /var/log/backup-2023-11-23.log
else
    echo "---error--- ${HOME}/bin/backup.py not found"
fi
