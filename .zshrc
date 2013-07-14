###############################################
## locate: ${XDG_CONFIG_HOME}/.zshrc         ##
## author: milomouse (github.com/milomouse)  ##
## detail: main configuration file for `zsh' ##
###############################################

# source external configuration files:
for i in ${HOME}/zsh/zsh-{options,exports,aliases,functions}; do
  . $i
done

# prompt line:
[[ ${TERM} =~ screen ]] && precmd() { print -Pn "\e]2;%2d\a" } #|| RPROMPT='%F{white}%~%f'
PS1='%(1j.%B%F{black}%j .)%(0?..%B%F{red}%? )%B%F{cyan}%#%f%b '
PS2='  '
PS3='%B%F{white}?# %b%f%F{red}%# %f'
PS4='%B%F{white}%_ %b%f%F{magenta}%# %f%B%F{white}+%N:%i %b%f%F{magenta}%# %f'

# auto-completion:
autoload -U compinit
compinit
_force_rehash() { (( CURRENT == 1 )) && rehash ; return 1 }
zstyle ':completion:::::' completer _force_rehash _expand _complete _approximate 
zstyle ':completion:*:descriptions' format "- %d -"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' verbose true
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ${XDG_CACHE_HOME:-${HOME}/cache}
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*:processes' command 'ps haxopid:5,user:4,%cpu:4,ni:2,stat:3,etime:8,args'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' force-list always


# framebuffer colors:
if [[ ${TERM} == linux ]] || [[ ${TERM} =~ screen && ${+DISPLAY} == 0 ]]; then
    echo -en "\e]P0000000" ; echo -en "\e]P83d3a3a" # 0: black/default
    echo -en "\e]P1d770af" ; echo -en "\e]P9d28abf" # 1: red
    echo -en "\e]P278a45c" ; echo -en "\e]PA9acc79" # 2: green
    echo -en "\e]P3c8bc45" ; echo -en "\e]PBd0d26b" # 3: yellow
    echo -en "\e]P477b6c5" ; echo -en "\e]PC8fa7b9" # 4: blue
    echo -en "\e]P5a488d9" ; echo -en "\e]PDbd89de" # 5: magenta
    echo -en "\e]P67ac0af" ; echo -en "\e]PE6ec2a8" # 6: cyan
    echo -en "\e]P78d8d8d" ; echo -en "\e]PFdad3d3" # 7: white
fi

# source custom colors:
eval $(dircolors -b ${HOME}/.dir_colors)

# Fish-like syntax highlighting for ZSH:
if [[ -f ${HOME}/zsh/plugins/zsh-syntax-highlight/zsh-syntax-highlighting.zsh ]]; then
  . ${HOME}/zsh/plugins/zsh-syntax-highlight/zsh-syntax-highlighting.zsh

  # override some colors:
  ZSH_HIGHLIGHT_STYLES[default]='none'
  ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red,bold,underline'
  ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=green'
  ZSH_HIGHLIGHT_STYLES[alias]='fg=magenta,bold'
  ZSH_HIGHLIGHT_STYLES[builtin]='fg=magenta,bold'
  ZSH_HIGHLIGHT_STYLES[function]='fg=magenta,bold'
  ZSH_HIGHLIGHT_STYLES[command]='fg=magenta'
  ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=red,bold,standout'
  ZSH_HIGHLIGHT_STYLES[path]='fg=white,underline'
  ZSH_HIGHLIGHT_STYLES[globbing]='fg=white,bold'
  ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=yellow'
  ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[assign]='fg=yellow,bold'
  ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=red,bold'
  ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=blue,bold'

  # override colors for matching brackets:
  ZSH_HIGHLIGHT_MATCHING_BRACKETS_STYLES=(
    'fg=blue,bold'    # Style for first level of imbrication
    'fg=green,bold'   # Style for second level of imbrication
    'fg=magenta,bold' # etc... Put as many styles as you wish,
    'fg=cyan,bold'    # or leave empty to disable brackets matching.
    'fg=white,bold'
    'fg=blue'
    'fg=green'
    'fg=magenta'
    'fg=cyan'
    'fg=white'
  )
fi

# Fish-like history sub-string search for ZSH (load AFTER syntax):
if [[ -f ${HOME}/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh ]]; then
  . ${HOME}/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh

  HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=white,fg=black,bold'
  HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=black'
  HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS='i'
fi

# keybindings (defined AFTER scripts):
bindkey "^[[2~" overwrite-mode
bindkey "^[[3~" delete-char
bindkey "^[[5~" up-line-or-search
bindkey "^[[6~" down-line-or-search
bindkey "^[[1~" beginning-of-line
bindkey "^[[7~" beginning-of-line
bindkey "^[[4~" end-of-line
bindkey "^[[8~" end-of-line
bindkey "^?" backward-delete-char
bindkey '^R' history-incremental-search-backward
