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

function ftsearch ()
{
    # full text search, searches target for a term
    local TERM="$1"
    local TARGET="$2"
    vim $(rg "$TERM" "$TARGET" | fzf | cut -d ":" -f 1)
}

function gitssh-work ()
{
    export GIT_SSH_COMMAND="ssh -i ~/.ssh/tuw_id_ed25519"
    git config --global user.name "Adam McCartney"
    git config --global user.email "adam.mccartney@tuwien.ac.at"
    git config --global user.signingkey 174C3ECBC22F87A8207AC9FE31DF3F14F2A9C47F
}

function gitssh-default ()
{
    export GIT_SSH_COMMAND="ssh -i ~/.ssh/id_ed25519"
    git config --global user.name "Adam McCartney"
    git config --global user.email "adam@mur.at"
    git config --global user.signingkey C5BF27EE0290CDE5BC8A8801A5FCE0B0A42EFDA8
}

function json_to_env ()
{
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

function mk_pyvenv () {
    NAME="$1"
    python -m venv "~/.virtualenvs/${NAME}"
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
    local WATCH=$1
    local TESTS=$2
    while inotifywait -q ${WATCH};
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
      rsync -auv -r -P  --ignore-existing --exclude="/.*" --exclude-from=$EXCLUDES_PATH --include="/.ssh" --include="/.password-store" ./ $BACKUP_PATH
}

function backup () {
    export CURRENTDATE=`date +"%b%d%Y"`
    local BACKUPTYPE="$1"

    if [ ! -d "/run/media/adam/adb/backup/$BACKUPTYPE" ]; then
        mkdir -p "/run/media/adam/adb/backup/$BACKUPTYPE"
        export BACKUP_DIR="/run/media/adam/adb/backup/$BACKUPTYPE"
    fi

    EXCLUDES_LIST="$HOME/backup_excludes.txt"

    backup_home $BACKUP_DIR $EXCLUDES_LIST
}

# Work related stuff for creating dev environments 

function load_aliases () 
{
    source "$HOME/dotfiles/bash/tu.aliases.bash"
}

function load_virtual_env () 
{
    local env_name=$1
    conda activate "$env_name"
}

function start_grader_service () 
{
    load_virtual_env "migration"
    cdlx
    grader-service -f "$GRADER_SERVICE_CONIG_PATH"
}

function create_gs_dev_session () 
{
    # creates a new tmux sesssion with a window for each of the following:
    local VENV=$1
    load_aliases
    ## 1. editor  
    tmux new-session -s gs-dev -n editor -d
    tmux send-keys -t gs-dev "conda activate $VENV" C-m
    tmux send-keys -t gs-dev "ltucfg" C-m
    tmux send-keys -t gs-dev "vim" C-m
    # 2. database
    tmux new-window -t gs-dev -n database
    tmux send-keys -t gs-dev:2.1 'ltucfg' C-m
    tmux send-keys -t gs-dev:2.1 'dbconnect' C-m
    # 3. grader labextension 
    tmux new-window -t gs-dev -n labext 
    tmux send-keys -t gs-dev:3.1 "conda activate $VENV" C-m
    tmux send-keys -t gs-dev:3.1 "ltucfg" C-m
    tmux send-keys -t gs-dev:3.1 "cdlx" C-m
    tmux send-keys -t gs-dev:3.1 "jlpm build" C-m
    # 4. jupyter apps (2 panes - lab and grader-service)
    tmux new-window -t gs-dev -n srv 
    tmux split-window -v -t srv 
    tmux send-keys -t gs-dev:4.1 "conda activate $VENV" C-m
    tmux send-keys -t gs-dev:4.1 "ltucfg" C-m
    tmux send-keys -t gs-dev:4.1 "cdgs" C-m 
    tmux send-keys -t gs-dev:4.1 "jupyterhub -f $JUPYTERHUB_CONFIG_PATH" C-m
    tmux send-keys -t gs-dev:4.2 "conda activate $VENV" C-m
    tmux send-keys -t gs-dev:4.2 "ltucfg" C-m
    tmux send-keys -t gs-dev:4.2 "cdlx" C-m
    tmux send-keys -t gs-dev:4.2 "grader-service -f $GRADER_SERVICE_CONFIG_PATH" C-m
    tmux select-window -t gs-dev:1 
    tmux attach -t gs-dev
}

function create_pydev_repl_session ()
{
    # creates a new tmux sesssion with a window for each of the following:
    local VENV=$1
    ## 1. editor  
    tmux new-session -s dev -n editor -d
    tmux split-window -hf -t dev 
    tmux send-keys -t dev:1.1 "conda activate $VENV" C-m
    tmux send-keys -t dev:1.1 "vim" C-m
    tmux send-keys -t dev:1.2 "conda activate $VENV" C-m
    tmux send-keys -t dev:1.2 "python3" C-m
    tmux attach -t dev
}
