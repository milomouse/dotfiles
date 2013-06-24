# Fuck bash.
source $H/conf/zsh/zsh-exports
source $H/conf/zsh/zsh-aliases
export HISTFILE=/tmp/user-keep/${USER}/bash_history
complete -cf sudo
PS1='\u [\w]\$ '
