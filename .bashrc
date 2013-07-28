# Fuck bash.
source ~/zsh/zsh-exports
source ~/zsh/zsh-aliases
export HISTFILE=/tmp/user-keep/${USER}/bash_history
complete -cf sudo
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
  source /usr/share/bash-completion/bash_completion
PS1='\u [\w]\$ '
