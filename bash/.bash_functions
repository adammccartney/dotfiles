# ~/.bash_functions: collection of command line functions
  # useage: source via ~/.bashrc at runtime

function swap_ctrl_caps () {
      XKBOPTIONS="ctrl:swapcaps"
      if command /usr/bin/gsettings &> /dev/null; then
          /usr/bin/gsettings set org.gnome.desktop.input-sources xkb-options "['caps:ctrl_modifier']"
      fi
      if command /usr/bin/setxbmap &> /dev/null; then
          /usr/bin/setxkbmap -option $XKBOPTIONS
      fi
    }

function json_to_env () {
    # Converts json dict to an env file with key=value pairs.

    # require jq binary
    if ! command -v jq &> /dev/null
       then
           echo "Error: no jq, (you may be better off without it)"
           return -1
    fi
    # if argc != 2 (more or less, but in crazy bash talk)
    if [ -z "$1" ] || [  -z "$2" ]; then
        echo "$_DOC"
        return -1
    fi
    local FILENAME=$1
    local ENVFILE=$2
    cat $FILENAME | jq -r 'keys[] as $k | "export \($k)=\(.[$k])"' > $ENVFILE
}

function orgtomd () {
    local _DOC="usage: orgtomd <input> <output>"
    if ! command -v pandoc &> /dev/null
    then
        echo "Error: pandoc could not be found, please install."
        return -1
    fi
    if [ -z "$1" ] || [  -z "$2" ]; then
        echo "$_DOC"
        return -1
    fi
    input=$1
    output=$2
    pandoc -w markdown -o $output -f org $input
}

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

function make_html_onchange () {
    # Run from the 'project/docs' directory
    while inotifywait -q ./source ; do echo -e '\n\n'; make html; done
    # Watch the source directory, if there are any changes, remake the docs.
    }

function serve_html_docs () {
    # Run from the 'project/docs' directory
    cd ./build/html && python3 -m http.server
    # Serve the html docs built by sphinx
}

function pytestonchange () {
    TESTS=$1
    while inotifywait -q ${TESTS};
    do
        echo -e '\n\n';
        pytest ${TESTS} -v;
    done
}

getmail () {
    mbsync -a
}

adworkon () {
    VENV=$1
    VENV_PATH=$HOME/.virtualenv/$VENV
    if [ ! -d "$VENV_PATH" ]
    then
        echo "Directory $VENV_PATH DOES NOT exist."
    else
        source $VENV_PATH/bin/activate
    fi

    }

function backup_home () {
      # WARNING: assumes that you are running from home!
      BACKUP_PATH=$1
      EXCLUDES_PATH=$2
      rsync -raP  --exclude="/.*" --exclude-from=$EXCLUDES_PATH --include="/.ssh" --include="/.password-store" ./ $BACKUP_PATH
}

function backup () {
    export CURRENTDATE=`date +"%b%d%Y"`
    export BACKUPTYPE="diza"

    if [ ! -d "/media/adam/ADB/backup/$BACKUPTYPE/$CURRENTDATE/" ]; then
        mkdir -p /media/adam/ADB/backup/$BACKUPTYPE/$CURRENTDATE
        export BACKUP_DIR="/media/adam/ADB/backup/$BACKUPTYPE/$CURRENTDATE"
    fi

    EXCLUDES_LIST="$HOME/backup_excludes.txt"

    backup_home $BACKUP_DIR $EXCLUDES_LIST
}
