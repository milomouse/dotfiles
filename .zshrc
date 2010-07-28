# extend run-path for local scripts/binaries:
PATH=$PATH:/usr/local/bin:/usr/local/sbin

# start tmux server(s) per tty$:
if [[ -z "$DISPLAY" && $(tty) = /dev/tty[2-4] ]]; then
  case $(tty) in
    *[2-4]) tmux -f ${XDG_CONFIG_DIR:-$HOME/.config}/tmux/tmux.conf \
    -L console new-session -s "$(print ${$(tty)/*dev\//})" ;;
  esac
fi

# prompt line:
[[ "$TERM" == screen* ]] && precmd() {print -Pn "\e]2;%2d\a"} || RPROMPT='%F{white}%~%f'
PS1='%F{magenta}» %f'
PS2='%B%F{white}%_ %b%f%F{magenta}» %f'
PS3='%B%F{white}?# %b%f%F{magenta}» %f'
PS4='%B%F{white}%_ %b%f%F{magenta}» %f%B%F{white}+%N:%i %b%f%F{magenta}» %f'

# auto-completion:
autoload -U compinit
compinit
_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1
}
zstyle ':completion:::::' completer _force_rehash _expand _complete _approximate 
zstyle ':completion:*:descriptions' format "- %d -"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' verbose true
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ${XDG_CACHE_HOME:-/dev/shm}/zsh
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -A -o pid,user,state,pcpu,etime,cmd'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' force-list always
[[ ! -a $(which bauerbill) ]] || compdef _pacman bauerbill=pacman
[[ ! -a $(which pacman-color) ]] || compdef _pacman pacman-color=pacman

# optimizations:
setopt VI
setopt AUTO_CD
setopt CHASE_LINKS
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt HASH_LIST_ALL
setopt LIST_TYPES
setopt LIST_PACKED
setopt NO_MATCH
setopt CORRECT
setopt COMPLETE_IN_WORD
setopt HIST_VERIFY
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_NO_STORE
setopt HIST_NO_FUNCTIONS
setopt HIST_FIND_NO_DUPS
setopt NO_CLOBBER
setopt EXTENDED_GLOB
setopt NUMERIC_GLOB_SORT
setopt DOT_GLOB
setopt NO_BEEP
setopt NOTIFY
setopt CHECK_JOBS
setopt LONG_LIST_JOBS
setopt AUTO_CONTINUE
setopt INTERACTIVE_COMMENTS
setopt WARN_CREATE_GLOBAL
setopt PROMPT_SUBST
unsetopt TRANSIENT_RPROMPT
unset MAILCHECK

# keybindings:
bindkey "^[[2~" overwrite-mode
bindkey "^[[3~" delete-char
bindkey "^[[5~" up-line-or-search 
bindkey "^[[6~" down-line-or-search 
bindkey "^[[7~" beginning-of-line
bindkey "^[[8~" end-of-line
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line
bindkey "^?" backward-delete-char
bindkey '^R' history-incremental-search-backward

# global exports:
[[ -z "$DISPLAY" ]] && export EDITOR="/usr/bin/vim -p -c ':colorscheme candymouse'" \
|| export EDITOR="/usr/bin/vim -p"
export HISTSIZE=1400
export SAVEHIST=$HISTSIZE
export DIRSTACKSIZE=20
export FCEDIT="/usr/bin/vim"
export BROWSER="/usr/bin/w3m"
export PAGER="/bin/less"
export SDCV_HISTSIZE=$HISTSIZE
export SDCV_PAGER="/bin/more"
export SHELL="/bin/zsh"
export MPD_HOST=lenovo
export MPD_PORT=6600
export LANG="en_US.utf8"
export LC_ALL="en_US.utf8"
export LC="en_US.utf8"
export LESSCHARSET="utf-8"
export LESSHISTFILE="${XDG_CACHE_HOME:-/dev/shm}/less_history"
export LESS_TERMCAP_mb=$'\E[01;35m'
export LESS_TERMCAP_md=$'\E[01;35m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;46;30m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;34m'
export XDG_CACHE_HOME="/dev/shm/cache"
export XDG_CONFIG_DIR="$HOME/.config"
export XDG_CONFIG_DIRS="$HOME/.config:/etc"
export XDG_DATA_HOME="$HOME/.config/share"
export XDG_DESKTOP_DIR="/dev/shm"
export XDG_DOCUMENTS_DIR="$HOME/rite"
export XDG_DOWNLOAD_DIR="$HOME/down"
export XDG_MUSIC_DIR="$HOME/muzk"
export XDG_PICTURES_DIR="$HOME/foto"
export XDG_PUBLICSHARE_DIR="/dev/shm"
export XDG_TEMPLATES_DIR="/dev/shm"
export XDG_VIDEOS_DIR="$HOME/vide"
export XAUTHORITY="$HOME/.config/.Xauthority"

# source alias and function files:
[[ -f ${XDG_CONFIG_DIR:-$HOME/.config}/zsh/zshalias ]] && . ${XDG_CONFIG_DIR:-$HOME/.config}/zsh/zshalias
[[ -f ${XDG_CONFIG_DIR:-$HOME/.config}/zsh/zshfn ]] && . ${XDG_CONFIG_DIR:-$HOME/.config}/zsh/zshfn

# framebuffer colors:
if [[ "$TERM" = "linux" || "$TERM" == *screen* ]]; then
    echo -en "\e]P0000000"
    echo -en "\e]P8171717"
    echo -en "\e]P19c8093"
    echo -en "\e]P9dfa6bb"
    echo -en "\e]P2799c99"
    echo -en "\e]PA85afa9"
    echo -en "\e]P3b0ad90"
    echo -en "\e]PBc4c497"
    echo -en "\e]P487afd7"
    echo -en "\e]PC98a7b6"
    echo -en "\e]P5a488d9"
    echo -en "\e]PD9f8bab"
    echo -en "\e]P66c7373"
    echo -en "\e]PEa3babf"
    echo -en "\e]P7999999"
    echo -en "\e]PF98a7b6"
fi
