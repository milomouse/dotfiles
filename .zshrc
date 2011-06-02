# source external configuration files:
ZDOTDIR="${HOME}/zsh"
for i in ${ZDOTDIR}/{options,exports,aliases,functions}; do
  . $i || { print "$i: cannnot source file" && setopt warncreateglobal }
done

# prompt line:
[[ ${TERM} =~ screen ]] && precmd() { print -Pn "\e]2;%2d\a" } || RPROMPT='%F{white}%~%f'
PS1='%F{magenta}%# %f'
PS2='%F{blue}%# %f'
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
[[ -a $(whence -p pacman-color) ]] && compdef _pacman pacman-color=pacman

# keybindings:
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

# framebuffer colors:
if [[ ${TERM} == linux ]] || [[ ${TERM} =~ screen && ${+DISPLAY} == 0 ]]; then
    echo -en "\e]P0000000" ; echo -en "\e]P83d3a3a"
    echo -en "\e]P1d74b73" ; echo -en "\e]P9e07895"
    echo -en "\e]P2799c99" ; echo -en "\e]PA85afa9"
    echo -en "\e]P3c8bc45" ; echo -en "\e]PBbaa02c"
    echo -en "\e]P476ace2" ; echo -en "\e]PC98a7b6"
    echo -en "\e]P5a488d9" ; echo -en "\e]PD9f8bab"
    echo -en "\e]P6508686" ; echo -en "\e]PE569e9a"
    echo -en "\e]P78d8d8d" ; echo -en "\e]PFdad3d3"
fi

# Fish-style syntax-highlighting for ZSH (by nicoulaj@github):
if [[ -f ${ZDOTDIR}/scripts/zsh-syntax-highlighting.zsh ]]; then
  . ${ZDOTDIR}/scripts/zsh-syntax-highlighting.zsh

  # override some colors:
  ZSH_HIGHLIGHT_STYLES[default]='none'
  ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red,standout'
  ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=magenta'
  ZSH_HIGHLIGHT_STYLES[alias]='fg=magenta'
  ZSH_HIGHLIGHT_STYLES[builtin]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[function]='fg=magenta,bold'
  ZSH_HIGHLIGHT_STYLES[command]='fg=cyan'
  ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=cyan,standout'
  ZSH_HIGHLIGHT_STYLES[path]='fg=blue,bold'
  ZSH_HIGHLIGHT_STYLES[globbing]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=red,bold'
  ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=red,bold'
  ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=cyan'
  ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=cyan'
  ZSH_HIGHLIGHT_STYLES[assign]='fg=red'

  # override colors for matching brackets:
  ZSH_HIGHLIGHT_MATCHING_BRACKETS_STYLES=(
    'fg=blue,bold'    # Style for first level of imbrication
    'fg=green,bold'   # Style for second level of imbrication
    'fg=magenta,bold' # etc... Put as many styles as you wish, or leave
    'fg=red,bold'  # empty to disable brackets matching.
    'fg=cyan,bold'
  )
fi
