#!/bin/bash

source /home/john_vm/.xmonad/gd.sh
rclone sync "$local" "$remote" --progress
