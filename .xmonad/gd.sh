#!/bin/bash

# Check number of arguments
if [ "$#" -ne 1 ]; then
    echo "Error: Invalid number of arguments"
    echo "Use \"gd pull\", \"gd push\", \"gd pull_copy\" or \"gd push_copy\""
    return
fi

# Get the current working directory
cwd=$(pwd)
# Extract the relative part
dr1=${cwd//$HOME\//}

# Determine remote (plain or encrypted)
rmt=$(echo $dr1 | awk -F '/' '{print $1}')
if [ $rmt == "google_drive" ]; then
    dr=${cwd//$HOME\/google_drive/}
    local=$HOME/google_drive$dr
    remote=gd:plain$dr
elif [ $rmt == "google_drive_encr" ]; then
    dr=${cwd//$HOME\/google_drive_encr/}
    local=$HOME/google_drive_encr$dr
    remote=gd_encr:$dr
elif [ $rmt == "google_drive_shared" ]; then
     dr=${cwd//$HOME\/google_drive_shared/}
     local=$HOME/google_drive_shared$dr
     remote=gd:shared$dr
else
    echo "Not a cloud folder: $rmt"
    return
fi

# Determine function (pull, push, pull_copy or push_copy)
if [ $1 == "pull" ]; then
    rclone sync "$remote" "$local" --progress --update
elif [ $1 == "push" ]; then
    rclone sync "$local" "$remote" --progress --update
elif [ $1 == "pull_copy" ]; then
    rclone copy "$remote" "$local" --progress --update
elif [ $1 == "push_copy" ]; then
    rclone copy "$local" "$remote" --progress --update
else
    echo "Invalid function: $1"
    echo "Use \"gd pull\", \"gd push\", \"gd pull_copy\" or \"gd push_copy\""
fi
