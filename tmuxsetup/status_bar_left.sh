#!/bin/bash
tmux ls -F "#{?session_attached,\
#[fg=colour3]#S#[fg=colour58]:#[fg=yellow]#I#[fg=colour58](#{session_windows})\
#[fg=cyan]#P#[fg=colour23](#{window_panes}),\
#[fg=colour58]#S"\
}\
| tr "\\n" " "
#[fg=colour23]#S#[fg=colour58](#{session_windows})"\
