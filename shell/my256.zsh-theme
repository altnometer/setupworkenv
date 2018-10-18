# worked out from robbyrussle.zsh-theme
local ret_status="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )"
function parse_tmux_session {
    tty=$(tty); for s in $(tmux list-sessions -F '#{session_name}' 2>/dev/null); do
    tmux list-panes -F '#{pane_tty} #{session_name}' -t "$s"; done | grep "$tty\>" | awk '{print $2}'
}
#PROMPT='${ret_status} %F{024}%n%f%F{029}@%f%F{024}%m%f%F{037}[$(parse_tmux_session)]%f%F{132}%c/%f%{$reset_color%}$(git_prompt_info) %B%#%b '
PROMPT='${ret_status} %F{024}%n%f%F{029}@%f%F{024}%m%f%F{132}[$(parse_tmux_session)]%f%F{037}%c/%f%{$reset_color%}$(git_prompt_info) %B%#%b '

ZSH_THEME_GIT_PROMPT_PREFIX="%F{166}(%f%F{172}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%F{166})%f%F{160}✗%f"
ZSH_THEME_GIT_PROMPT_CLEAN="%F{166})%f"
