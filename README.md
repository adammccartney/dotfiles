# dotfiles

Mostly editor config stuff and a few other manifests for setting up my desktop systems.
If guix home is available, this can (nearly) be used to manage everything.
Note that it is still a little bit buggy on foreign distros.

[[https://www.gnu.org/software/stow/][Stow]] is a symlink farm manager and works
surprisingly well and is an improvement over the pattern of git
submodules and multiple git repos to manage the various configs. 
[[http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html][Here is a good introduction to stow]]


# Usage

Cloning the repo is the first step in any case:

```
git clone https://github.com/adammccartney/dotfiles
```

## no-guix-home

If we are on a system where guix home is not available, there are a couple 
of wrapper scripts to make it easier to manage the config files using show.

To initialize a new *$HOME*, run `cd ~/dotfiles && make stow-no-guix-home`

To restow: `cd ~/dotfiles && make restow-no-guix-home`

To delete: `cd ~/dotfiles && make unstow-no-guix` 


## guix home

Note: guix home on a foreign distro has not been tested with this setup.
For guix system, simply run `guix home reconfigure` with the desired
config.
