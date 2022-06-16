#######################################################
## locate: ${XDG_CONFIG_HOME}/.zshrc                 ##
## author: Vincent (github.com/milomouse)            ##
## detail: main configuration file for `zsh'         ##
#######################################################

# source external configuration files:
for i in ${HOME}/zsh/zsh-{options,exports,aliases,functions} /usr/share/fzf/key-bindings.zsh ; do
  if [[ -f $i ]] { source $i } else { print "Cannot find file: $i" }
done

# prompt line:
[[ ${TERM} =~ screen ]] && precmd() { print -Pn "\e]2;%2d\a" }
function zle-keymap-select zle-line-init zle-line-finish
{
  if [[ $KEYMAP == (viins|main) ]] ; then
    _VP1="{cyan}MOTHER%b%F{white}"
    _VP2="{green}"
  else
    _VP1="{black}MOTHER%b%F{red}"
    _VP2="{black}"
  fi
  zle reset-prompt
  zle -R
}
PS1='%(1j.%B%F{black}%j .)%(0?..%B%F{red}%? )%B%F${_VP1}%#%b%f '
PS2='%B%F${_VP2}> %b%f'
PS3='%B%F{white}?# %b%f%F{red}%# %f'
PS4='%B%F{white}%_ %b%f%F{magenta}%# %f%B%F{white}+%N:%i %b%f%F{magenta}%# %f'
zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

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
  echo -en "\e]P0000000" ; echo -en "\e]P83D3A3A" # 0: black/default
  echo -en "\e]P1E31763" ; echo -en "\e]P9C2003B" # 1: red
  echo -en "\e]P27DE1D3" ; echo -en "\e]PA27C8B1" # 2: green
  echo -en "\e]P3EAE900" ; echo -en "\e]PBBFC521" # 3: yellow
  echo -en "\e]P46186B7" ; echo -en "\e]PC1D43B2" # 4: blue
  echo -en "\e]P5A538FF" ; echo -en "\e]PD8600E2" # 5: magenta
  echo -en "\e]P6BAE5DB" ; echo -en "\e]PE73B8A8" # 6: cyan
  echo -en "\e]P79E9DA5" ; echo -en "\e]PFA9B1A6" # 7: white
fi

# source custom colors:
eval $(dircolors -b ${HOME}/.dir_colors)

# Fish-like syntax highlighting for ZSH:
if [[ -f $HOME/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
  source $HOME/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

  # activate highlighters:
  ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

  # override main colors:
  ZSH_HIGHLIGHT_STYLES[default]='none'
  ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red,bold'
  ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=blue,bold'
  ZSH_HIGHLIGHT_STYLES[assign]='fg=yellow,bold'
  ZSH_HIGHLIGHT_STYLES[alias]='fg=white'
  ZSH_HIGHLIGHT_STYLES[function]='fg=white'
  ZSH_HIGHLIGHT_STYLES[builtin]='fg=magenta'
  ZSH_HIGHLIGHT_STYLES[command]='fg=magenta'
  ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=red,bold,standout'
  ZSH_HIGHLIGHT_STYLES[path]='fg=white,underline'
  ZSH_HIGHLIGHT_STYLES[path_prefix]='fg=white,underline'
  ZSH_HIGHLIGHT_STYLES[path_approx]='fg=green,bold'
  ZSH_HIGHLIGHT_STYLES[globbing]='fg=yellow,bold'
  ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=yellow'
  ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=red,bold'
  ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=blue,bold'

  # override bracket colors:
  ZSH_HIGHLIGHT_STYLES[bracket-error]='fg=red,bold'
  ZSH_HIGHLIGHT_STYLES[bracket-level-1]='fg=green,bold'
  ZSH_HIGHLIGHT_STYLES[bracket-level-2]='fg=white,bold'
  ZSH_HIGHLIGHT_STYLES[bracket-level-3]='fg=blue,bold'
  ZSH_HIGHLIGHT_STYLES[bracket-level-4]='fg=green,bold'
  ZSH_HIGHLIGHT_STYLES[bracket-level-5]='fg=white,bold'
  ZSH_HIGHLIGHT_STYLES[bracket-level-6]='fg=blue,bold'

  # override pattern decorations:
  ZSH_HIGHLIGHT_PATTERNS+=('rm -[f,r] *' 'fg=black,bg=white,bold')
  ZSH_HIGHLIGHT_PATTERNS+=('rm -[f,r][f,r] *' 'fg=black,bg=white,bold')
  ZSH_HIGHLIGHT_PATTERNS+=('shred *' 'fg=black,bg=white,bold')

fi

# Fish-like history sub-string search for ZSH (load AFTER syntax):
if [[ -f $HOME/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh ]]; then
  source $HOME/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh

  # override main colors:
  HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=white,fg=black,bold'
  HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=black'
  HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS='i'

  # bind UP/DOWN in normal mode:
  zmodload zsh/terminfo
  bindkey "$terminfo[kcuu1]" history-substring-search-up
  bindkey "$terminfo[kcud1]" history-substring-search-down

  # bind K/J for VI mode:
  bindkey -M vicmd 'k' history-substring-search-up
  bindkey -M vicmd 'j' history-substring-search-down
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
bindkey -M vicmd '^U' vi-kill-line
bindkey -M viins '^U' kill-whole-line
