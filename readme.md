# config-1

My configuration files.

## Info

I use a git bare repository to track changes to specific configuration files. This setup has a lot of benefits, like being able to track changes, having an organized collection of all the important configuration files where manual changes have been made, and making it easier to reconfigure things after a reinstall or system migration.

## Replicating this setup

Adapted from [Nicola Paolucci's tutorial](https://www.atlassian.com/git/tutorials/dotfiles).

Making a git bare repository and an alias:
```
$ mkdir $HOME/dotfiles
$ git init --bare $HOME/dotfiles
$ echo "alias config='/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME'" >> $HOME/.bashrc
```
Now `config` can be issued instaed of `git`, and the repository can be accessed from any directory.

Restart:
```
$ bash
```
Don't show untracked files.
```
$ config config --local status.showUntrackedFiles no
```
Add, remove, commit, etc etc.
```
$ config status
$ config add <file>
$ config commit -m "<message>"
$ config push orogin main
```

# Port the dotfiles on a new device

```
$ mkdir $HOME/dotfiles
$ git init --bare $HOME/dotfiles
$ echo "alias config='/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME'" >> $HOME/.bashrc

config remote add origin https://github.com/ioannis-vm/config-1
config pull origin main
```
