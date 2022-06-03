#!/bin/bash

# Script to interact with Berkeley HPC

username=ioannisvm
login_addr='hpc.brc.berkeley.edu'
transfer_addr='dtn.brc.berkeley.edu'

pin=$(gpg -dq ~/.pin.gpg)
out_str=$(GASHELL_PASSPHRASE=$pin gashell -o)
code=$(echo $out_str | awk -F ': ' '{print $NF}')

# Check number of arguments
helpstr="Use \"sv connect\", \"sv push\", \"sv pull\",..."

if [ "$#" -ne 1 ]; then
    echo "Error: Invalid number of arguments"
    echo $helpstr
    return
fi

# Check internet connectivity
wget -q --spider http://google.com
if [ $? -ne 0 ]; then
    echo "Offline"
    return
fi

if [ $1 == 'connect' ]; then
    echo "Connecting to login node"
    TERM=xterm-256color sshpass -p $pin$code ssh -Y $username@$login_addr
    # https://github.com/alacritty/alacritty/issues/3962
    return 0
fi

# Get the current working directory
cwd=$(pwd)
# Get the parent directory
cd ../ && pd=$(pwd) && cd $cwd

# make sure we are in the berkeley_hpc directory
if ! [[ $cwd == *"/home/john_vm/berkeley_hpc"* ]]; then
    echo "Error: Not in the right directory!"
    read -p "Change directory? [y/n]" -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
	cd "/home/john_vm/berkeley_hpc"
	return 0
    else
	return 1
    fi
fi

local_base_path="$HOME/berkeley_hpc"

if [ "$cwd" = "/home/john_vm/berkeley_hpc" ]; then
    # we are in the parent directory
    remote_base_path='/global/home/users/ioannisvm'
    remote_cwd="$remote_base_path/berkeley_hpc"
    remote_pwd="$remote_base_path"
else
    # we are in a subdir
    cwd_rel=${cwd//$local_base_path\//}
    pwd_rel=${pwd//$local_base_path\//}
    remote_base_path='/global/home/users/ioannisvm/berkeley_hpc'
    remote_cwd="$remote_base_path/$cwd_rel"
    remote_pwd="$remote_base_path/$pwd_rel"
fi


if [ $1 == 'push' ]; then
    sshpass -p $pin$code rsync --update -arvz -e 'ssh' --progress --delete $cwd $username@$transfer_addr:$remote_pwd
elif [ $1 == 'pull' ]; then
    sshpass -p $pin$code rsync --update -arvz -e 'ssh' --progress --delete $username@$transfer_addr:$remote_cwd $pd
elif [ $1 == 'push_copy' ]; then
    sshpass -p $pin$code rsync --update -arvz -e 'ssh' --progress $cwd $username@$transfer_addr:$remote_pd
elif [ $1 == 'pull_copy' ]; then
    sshpass -p $pin$code rsync --update -arvz -e 'ssh' --progress $username@$transfer_addr:$remote_cwd $pd
else
    echo "Invalid function: $1"
    echo $helpstr
fi
