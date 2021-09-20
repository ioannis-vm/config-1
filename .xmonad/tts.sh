#!/bin/bash
#
# Speak what's in the clipboard
#
# Copy text, run script, have it read by Google Translate's TTS API
#

# Dependencies:
# xclip (available in Ubuntu 20.04)
# vlc   (available in Ubuntu 20.04)
# gTTS (python -m pip install gTTS)


audio_file="$HOME/.xmonad/.tts.mp3"

clipboard=$(xclip -o -selection clipboard)
/home/john_vm/anaconda3/bin/gtts-cli "$clipboard" --output "$audio_file"
vlc --rate 1.3 "$audio_file"
