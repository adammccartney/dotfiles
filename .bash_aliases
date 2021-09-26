# backup system
alias backup="rsync -a --exclude=.cache --progress /home/adam /media/adam/adb/backup/home/adam"

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

# wagtail python 3.8
alias wtpy38="source ~/.virtualenvs/wtpy38/bin/activate"

# django: postgres python38
alias pstgrsql="source ~/.virtualenvs/postgresql/bin/activate"

# neovim
alias vim="/usr/local/bin/nvim"
