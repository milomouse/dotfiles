# prompt line
precmd() {print -Pn "\e]2;%2d\a"}
PS1='%F{red}» %f'
PS2='%B%F{white}%_ %b%f%F{red}» %f'
PS3='%B%F{white}?# %b%f%F{red}» %f'
PS4='%B%F{white}%_ %b%f%F{red}» %f%B%F{white}+%N:%i %b%f%F{red}» %f'

# exports
export HISTSIZE=1400
export SAVEHIST=$HISTSIZE
export DIRSTACKSIZE=20
export FCEDIT=/usr/bin/vim
export EDITOR=/usr/bin/vim
export BROWSER=/usr/bin/w3m
export IMGV=/usr/bin/gliv
export VIDV=/usr/bin/mplayer
export PDFV=/usr/bin/zathura
export CC=/usr/bin/gcc
export SHELL=/bin/zsh
export MPD_HOST=lenovo
export MPD_PORT=6600
export PAGER=/bin/less
export LANG="en_US.utf8"
export LC_ALL="en_US.utf8"
export LC="en_US.utf8"
export LESSCHARSET="utf-8"
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;45;30m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;34m'

# auto-completion
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
zstyle ':completion:*' cache-path $HOME/.cache/zsh
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -A -o pid,user,state,pcpu,etime,cmd'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' force-list always

# optimize
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
setopt RM_STAR_WAIT
setopt PROMPT_SUBST
unsetopt TRANSIENT_RPROMPT
unset MAILCHECK
autoload -U pick-web-browser
zstyle ':mime:*' x-browsers firefox-nightly uzbl-browser
zstyle ':mime:*' tty-browsers w3m links

# make home/end keys work + other stuff
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

PATH=$PATH:/usr/sbin:/usr/local:/usr/local/sbin:/usr/local/bin

# source aliases
if [ -f ~/.zshalias ]; then
  . ~/.zshalias
fi

# source ssh
function agent-s {
  local SSH_ENV="$HOME/.ssh/environment"
  echo "Initializing new SSH agent ..."
  if [ -f ${SSH_ENV} ]; then
    rm ${SSH_ENV} >&/dev/null
  fi
  if [ ! `ps -a|grep -i ssh-agent` ]; then
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add;
    git pull origin master
  else
    killall ssh-agent &>/dev/null
  fi
}

# zenburn framebuffer colors
#if [ "$TERM" = "linux" ]; then
#    echo -en "\e]P01C1C20"
#    echo -en "\e]P84d4d4d"
#    echo -en "\e]P1CE5C00"
#    echo -en "\e]P9F57900"
#    echo -en "\e]P2B7CE42"
#    echo -en "\e]PABDE077"
#    echo -en "\e]P3B88B10"
#    echo -en "\e]PBFFC135"
#    echo -en "\e]P466AABB"
#    echo -en "\e]PCAACCBB"
#    echo -en "\e]P5B7416E"
#    echo -en "\e]PDBB4466"
#    echo -en "\e]P65E7175"
#    echo -en "\e]PEA3BABF"
#    echo -en "\e]P7D6D8D9"
#    echo -en "\e]PF6C887A"
#    clear
#fi
