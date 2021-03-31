# config-1

Configuration files for my ubuntu instances.

## Info

I use a git bare repository to track changes to specific configuration files. This setup has a lot of benefits, like being able to track changes, having an organized collection of all the important configuration files where manual changes have been made, and making it easier to reconfigure things after a reinstall or system migration.

## Replicating this setup

Adapted from [Nicola Paolucci's tutorial](https://www.atlassian.com/git/tutorials/dotfiles).

Make a git bare repository and an alias:
```
$ mkdir $HOME/dotfiles
$ git init --bare $HOME/dotfiles
$ echo "alias config='/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME'" >> $HOME/.bashrc
```

Restart:
```
$ bash
```
We are only interested in tracking the files we specify. We don't care about the rest.
```
$ config config --local status.showUntrackedFiles no
```
Use `config` instead of `git` to add, remove, commit, etc.
```
$ config status
```
```
$ config add <file>
```
```
$ config commit -m "<message>"
```
```
$ config push
```
