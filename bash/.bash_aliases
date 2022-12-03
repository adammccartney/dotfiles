## virtualenv alias
#alias sv="source env/bin/activate"
#
## IPython in a virtual env
#alias ipy="python -c 'import IPython; IPython.terminal.ipapp.launch_new_instance()'"

## frog virtual env alias (for Scores/rill)
#alias frogsv="source ~/Scores/frog/env/bin/activate"

# plot virtual env 
alias plot="source ~/.virtualenvs/plot/bin/activate"

# chi virtual env (for Django webversion of iChing)
alias chi="source ~/.virtualenvs/chi/bin/activate"

# ni virtual env (for abjad scores)
alias ni="source ~/.virtualenvs/ni/bin/activate"

# plot virtual env (for learning & iChing)
alias tt="source ~/.virtualenvs/tt/bin/activate"

# call an iChing reading
alias iching="python3 ~/Code/iChing/iChing/build/iching.py"

# fl venv
alias fl="source ~/.virtualenvs/fl/bin/activate"

# jlox
alias jlox="~/.local/src/jlox/build/distributions/jlox/bin/jlox"

# python3.8
alias py3.8="/usr/local/bin/python3.8"

# python 3.10
alias py310="/usr/local/bin/python3.10"

# wagtail python 3.8
alias wtpy38="source ~/.virtualenvs/wtpy38/bin/activate"

# django: postgres python38
alias pstgrsql="source ~/.virtualenvs/postgresql/bin/activate"

# neovim
alias vim="$(which nvim)"

# psql alias (hand compiled version on unix has the side effect of trying to
# connect with another port. To get around this, we have to connect using the
# -h flag and the socket that is specified by the debian system
alias psql="psql -h /var/run/postgresql"

alias DIZA="192.168.0.193"

alias tlog="$HOME/Documents/traininglogs/training22.md"

alias actenv="source venv/bin/activate"

alias train="source $HOME/bin/train"

alias !P="PS1='# '"
