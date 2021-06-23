#!/bin/bash

source /home/john_vm/.xmonad/gd.sh
rclone copy "$remote" "$local" --progress --update
