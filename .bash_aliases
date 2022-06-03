# aliases for color support

# # Add an "alert" alias for long running commands.  Use like so:
# #   sleep 10; alert
# alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias htop='htop -d 0.5'
alias config='/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME'
alias config2='/usr/bin/git --git-dir=/home/john_vm/dotfiles2 --work-tree=/home/john_vm'
alias ding='paplay /usr/share/sounds/freedesktop/stereo/complete.oga'
alias gd='. /home/john_vm/.xmonad/gd.sh'
alias ms='. /home/john_vm/.xmonad/ms.sh'
alias sv='. /home/john_vm/.xmonad/sv.sh'
alias home_alone='xrandr --output eDP-1 --mode 1920x1080 --output DP-1-2 --off --output DP-3 --off && nitrogen --restore  && xset r rate 200 100'
alias home_portrait='xrandr --output eDP-1 --mode 1920x1080 --output DP-1-2 --mode 2560x1440 --right-of eDP-1 --rotate left && nitrogen --restore  && xset r rate 200 100'
alias home_landscape='xrandr --output eDP-1 --mode 1920x1080 --output DP-1-2 --mode 2560x1440 --right-of eDP-1 --rotate normal && nitrogen --restore  && xset r rate 200 100'
alias office_portrait='xrandr --output eDP-1 --mode 1920x1080 --output DP-3 --mode 1920x1080 --right-of eDP-1 --rotate left && nitrogen --restore & xset r rate 200 100'
alias office_landscape='xrandr --output eDP-1 --mode 1920x1080 --output DP-3 --mode 1920x1080 --right-of eDP-1 --rotate normal && nitrogen --restore && xset r rate 200 100'
alias update_pkglists='pacman -Qqen > $(HOME)/pkglist.txt && pacman -Qqem > $(HOME)/foreignpkglist.txt'
alias lock_screen='xscreensaver-command -lock'
alias sync_time='timedatectl set-timezone "$(curl --fail https://ipapi.co/timezone)"'
alias matlab="/usr/local/MATLAB/R2019a/bin/matlab -softwareopengl"
alias matlabnw="$HOME/MATLAB/R2021b/bin/matlab -nodisplay"
alias open="xdg-open"
