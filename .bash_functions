# ~/.bash_functions: collection of command line functions


# Functions

ds () {
    echo "Disk Space Utilization For $HOSTNAME"
    df -h
}

hs () {
    echo "Home Space Utilzation For $USER"
    du -sh /home/*
}

extractLoxBin () {
    DISTDIR=/home/adam/.local/src/jlox/build/distributions
    TARGETDIR=${DISTDIR}
    tar -xf "${DISTDIR}/jlox.tar" -C "${TARGETDIR}"
}

function renameFilesRecursively () {

  SEARCH_PATH="$1"
  SEARCH="$2"
  REPLACE="$3"

  find ${SEARCH_PATH} -type f -name "*${SEARCH}*" | while read FILENAME ; do
      NEW_FILENAME="$(echo ${FILENAME} | sed -e "s/${SEARCH}/${REPLACE}/g")";
      mv "${FILENAME}" "${NEW_FILENAME}";
  done

}

function tmux_ns () {
    SESNAME="$1"
    tmux new-session -s $SESNAME -d
    tmux split-window -h
    tmux split-window -v
    tmux -2 attach-session -d 
}

function hello () {
    NAME="$1"
    echo "hello ${NAME}, how are you this evening?"
}
