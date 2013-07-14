# Fuck bash.
source ~/zsh/zsh-exports
source ~/zsh/zsh-aliases
export HISTFILE=/tmp/user-keep/${USER}/bash_history
complete -cf sudo
PS1='(bash..) \u [\w]\$ '
