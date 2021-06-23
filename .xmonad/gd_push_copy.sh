#!/bin/bash

source /home/john_vm/.xmonad/gd.sh
rclone copy "$local" "$remote" --progress --update
