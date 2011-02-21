# Fuck bash.
source $H/conf/zsh/exports
source $H/conf/zsh/aliases
export HISTFILE=/dev/shm/.${UID}/bash_history
alias e="exit"
complete -cf sudo
PS1='\u [\w]\$ '
