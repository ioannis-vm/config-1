if status is-interactive
    # Commands to run in interactive sessions can go here
end

set fish_greeting

source ~/.bash_aliases

neofetch

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /home/john_vm/anaconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<
