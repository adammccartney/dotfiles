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

function containerip () {
   sudo docker inspect −−format '{{ .NetworkSettings.IPAddress }}' "$@"
}

function make_py3.8venv () {
    NAME="$1"
    #python3.8 -m venv "~/.virtualenvs/${NAME}"
    echo "~/.virtualenvs/${NAME}"
}

# list all ports currently listening
get_listening_ports () {
    sudo lsof -i -P -n | grep LISTEN 
}

function installed {
    cmd=$(command -v "${1}")

    [[ -n "${cmd}" ]] && [[ -f "${cmd}" ]]
    return ${?}
}

function die {
    >&2 echo "Fatal: ${@}"
    exit 1
}


function wi { 
    test -n "$1" && stat --printf "%F\n" $1
    }


function size {
    t=0
    test -d "$1" && for n in $(find $1 \
    -type f -name '*.py' -print | \
    xargs stat --printf "%s "); do ((t+=n)); done; echo $t; 
}


function weather { 
    curl -s --connect-timeout 3 -m 5 http://wttr.in/$1 
}


# Elastic search functions
if [ -f ~/.elastic_fun ]; then
    . ~/.elastic_fun
fi

function makeonchange () {
    while inotifywait -q . ; do echo -e '\n\n'; make; done
}
