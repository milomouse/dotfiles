# extend run-path for local scripts/binaries:
[[ -n $(print $PATH|grep local/bin) ]] || PATH=$PATH:/usr/local/bin:/usr/local/sbin

# prompt line:
[[ "$TERM" == screen* ]] && precmd() {print -Pn "\e]2;%2d\a"} || RPROMPT='%F{white}%~%f'
PS1='%F{magenta}» %f'
PS2='%F{blue}» %f'
PS3='%B%F{white}?# %b%f%F{red}» %f'
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
zstyle ':completion:*' cache-path ${XDG_CACHE_HOME:-/dev/shm}
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*:processes' command 'ps haxopid:5,user:4,%cpu:4,ni:2,stat:3,etime:8,args'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' force-list always
[[ ! -a $(which bauerbill) ]] || compdef _pacman bauerbill=pacman
[[ ! -a $(which pacman-color) ]] || compdef _pacman pacman-color=pacman

# optimizations:
setopt VI \
AUTO_CD  CHASE_LINKS  AUTO_PUSHD  PUSHD_IGNORE_DUPS \
HASH_LIST_ALL  LIST_TYPES  LIST_PACKED \
NOTIFY  NO_BEEP  NO_MATCH  CORRECT  COMPLETE_IN_WORD \
HIST_VERIFY  HIST_REDUCE_BLANKS  HIST_IGNORE_SPACE  HIST_NO_STORE \
HIST_IGNORE_ALL_DUPS  HIST_EXPIRE_DUPS_FIRST  HIST_FIND_NO_DUPS  HIST_NO_FUNCTIONS \
NO_CLOBBER  EXTENDED_GLOB  NUMERIC_GLOB_SORT  DOT_GLOB \
CHECK_JOBS  LONG_LIST_JOBS  AUTO_CONTINUE \
INTERACTIVE_COMMENTS  WARN_CREATE_GLOBAL  PROMPT_SUBST
unsetopt TRANSIENT_RPROMPT

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
export HISTSIZE=1400
export SAVEHIST=${HISTSIZE}
export DIRSTACKSIZE=20
export EDITOR="/usr/bin/vim"
export FCEDIT="/usr/bin/vim"
export BROWSER="/usr/bin/w3m"
export HOMEPAGE="https://bbs.archlinux.org/search.php?action=show_new"
export PAGER="/bin/less"
export SDCV_HISTSIZE=${HISTSIZE}
export SDCV_PAGER="/bin/more"
export SHELL="/bin/zsh"
export MPD_HOST=lenovo
export MPD_PORT=6600
export LANG="en_US.utf8"
export LC_ALL="en_US.utf8"
export LC="en_US.utf8"
export LESSCHARSET="utf-8"
export LESSHISTFILE="${XDG_CACHE_HOME:-/dev/shm}/.less_history--${USER:-milo}"
export LESS_TERMCAP_mb=$'\E[01;35m'
export LESS_TERMCAP_md=$'\E[01;35m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;46;30m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;34m'
export XDG_CACHE_HOME="/dev/shm/cache"
export XDG_CONFIG_DIR="${HOME}/.config"
export XDG_CONFIG_DIRS="${HOME}/.config:/etc"
export XDG_DATA_HOME="${HOME}/.config/share"
export XDG_DESKTOP_DIR="/dev/shm"
export XDG_DOCUMENTS_DIR="${HOME}/rite"
export XDG_DOWNLOAD_DIR="${HOME}/down"
export XDG_MUSIC_DIR="${HOME}/muzk"
export XDG_PICTURES_DIR="${HOME}/foto"
export XDG_PUBLICSHARE_DIR="/dev/shm"
export XDG_TEMPLATES_DIR="/dev/shm"
export XDG_VIDEOS_DIR="${HOME}/vide"
export XAUTHORITY="${HOME}/.config/xorg/.Xauthority"

# source alias and function files:
[[ -f ${XDG_CONFIG_DIR:-${HOME}/.config}/zsh/zshalias ]] && \
. ${XDG_CONFIG_DIR:-${HOME}/.config}/zsh/zshalias
[[ -f ${XDG_CONFIG_DIR:-${HOME}/.config}/zsh/zshfn ]] && \
. ${XDG_CONFIG_DIR:-${HOME}/.config}/zsh/zshfn

# framebuffer colors:
if [[ "${TERM}" = "linux" || "${TERM}" == screen* ]]; then
#   candymouse:
    echo -en "\e]P0000000"
    echo -en "\e]P83d3a3a"
    echo -en "\e]P1d74b73"
    echo -en "\e]P9b94062"
    echo -en "\e]P2799c99"
    echo -en "\e]PA85afa9"
    echo -en "\e]P3c8bc45"
    echo -en "\e]PBbaa02c"
    echo -en "\e]P476ace2"
    echo -en "\e]PC98a7b6"
    echo -en "\e]P5a488d9"
    echo -en "\e]PD9f8bab"
    echo -en "\e]P6508686"
    echo -en "\e]PE569e9a"
    echo -en "\e]P78d8d8d"
    echo -en "\e]PFdad3d3"
fi
