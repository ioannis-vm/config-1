#!/bin/bash

cwd=$(pwd)
dr=${cwd//home\/john_vm\/google_drive\//}
local=/home/john_vm/google_drive$dr
remote=gd_encr:$dr
