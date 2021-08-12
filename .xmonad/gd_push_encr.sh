#!/bin/bash

source /home/john_vm/.xmonad/gd_encr.sh
rclone sync "$local" "$remote" --progress --update
