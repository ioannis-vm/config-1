# config-1

Configuration files for my ubuntu laptop.

# How I set things up

Make a git bare repository:
```
mkdir $HOME/dotfiles
git init --bare $HOME/dotfiles
echo "alias config='/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME'" >> $HOME/.bashrc
```

Restart:
```
bash
```

```
config config --local status.showUntrackedFiles no
```

```
config status
```

Add files:
```
config add <file>
```

```
config commit -m "<message>"
```

```
config push
```
