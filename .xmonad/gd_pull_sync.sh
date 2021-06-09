#!/bin/bash

source /home/john_vm/.xmonad/gd.sh
rclone sync $remote $local --progress
