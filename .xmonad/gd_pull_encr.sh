#!/bin/bash

source /home/john_vm/.xmonad/gd_encr.sh
rclone sync "$remote" "$local" --progress --update
