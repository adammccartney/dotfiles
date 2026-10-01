#!/usr/bin/sh
#
# wrapper for home backups

CMD="${HOME}/bin/backup.py --old-home=${HOME} --new-home=/run/media/${USER}/adb/backup/home/${USER} --excludes-file=${HOME}/bin/bk-excludes.txt"

if [[ $1 == "--dry-run" ]]; then
    CMD="${CMD} --dry-run"
    echo "${CMD}"
fi

if command ${HOME}/bin/backup.py -h > /dev/null; then
    LOGFILE=/var/log/backup-$(date --iso-8601).log
    sudo touch ${LOGFILE} && sudo chown ${USER}:${USER} ${LOGFILE}
    ${CMD} 2> ${LOGFILE}
else
    echo "---error--- ${HOME}/bin/backup.py not found"
fi
