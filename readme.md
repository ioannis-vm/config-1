# config-1

My configuration files.

## Info

I use a git bare repository to track changes to specific configuration files. This setup has a lot of benefits, like being able to track changes, having an organized collection of all the important configuration files where manual changes have been made, and making it easier to reconfigure things after a reinstall or system migration.

Adapted from [Nicola Paolucci's tutorial](https://www.atlassian.com/git/tutorials/dotfiles).

## Initial setup

1. Making a git bare repository and an alias:
```
mkdir $HOME/dotfiles
git init --bare $HOME/dotfiles
echo "alias config='/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME'" >> $HOME/.bashrc
```
Now `config` can be issued instaed of `git`, and the repository can be accessed from any directory.

2. Relaunch terminal:
```
bash
```
3. Don't show untracked files.
```
config config --local status.showUntrackedFiles no
```

## Everyday use

Add, remove, commit, etc etc.
```
config status
config add <file>
config commit -m "<message>"
config push origin main
```

## Use the existing git bare repo and dotfiles on a new device

Steps 1-3, backup existing local dotfiles if needed, and then
```
config remote add origin https://github.com/ioannis-vm/config-1
config pull origin main
```
