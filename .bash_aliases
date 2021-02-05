# backup system
alias backup="rsync -a --exclude=.cache --progress /home/adam /media/adam/adb/backup/home/adam"

## virtualenv alias
#alias sv="source env/bin/activate"
#
## IPython in a virtual env
#alias ipy="python -c 'import IPython; IPython.terminal.ipapp.launch_new_instance()'"

## frog virtual env alias (for Scores/rill)
#alias frogsv="source ~/Scores/frog/env/bin/activate"

## webscrape virtual env alias
alias scrape="source ~/.virtualenvs/scrape/bin/activate"

# plot virtual env (for learning & iChing)
alias plot="source ~/.virtualenvs/plot/bin/activate"

# ni virtual env (for abjad scores)
alias ni="source ~/.virtualenvs/ni/bin/activate"

# plot virtual env (for learning & iChing)
alias abj32="source ~/.virtualenvs/abjad3.2/bin/activate"

# call an iChing reading
alias iching="python3 ~/Code/iChing/iChing/build/iching.py"

