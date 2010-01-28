# prompt line
PROMPT='%F{red}%/%# %f'

# history specification
export HISTSIZE=1400
export SAVEHIST=$HISTSIZE
setopt appendhistory nomatch notify

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
setopt AUTO_CD
setopt CHASE_LINKS
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt HASH_LIST_ALL
setopt LIST_TYPES
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
setopt CHECK_JOBS
setopt LONG_LIST_JOBS
setopt AUTO_CONTINUE
setopt INTERACTIVE_COMMENTS
setopt promptsubst
setopt nobeep
unsetopt transient_rprompt
unset mailcheck
setterm -blength 0

# make home and end key work + some other goodies
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
SSH_ENV="$HOME/.ssh/environment"
 
function agent-s {
  echo "Initializing new SSH agent..."
  rm ${SSH_ENV} >&/dev/null
  /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
  echo succeeded
  chmod 600 "${SSH_ENV}"
  . "${SSH_ENV}" > /dev/null
  /usr/bin/ssh-add;
}
# automatically source
#if [ -f "${SSH_ENV}" ]; then
#  . "${SSH_ENV}" > /dev/null
#  ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
#    agent-s;
#  }
#else
#  agent-s;
#fi
