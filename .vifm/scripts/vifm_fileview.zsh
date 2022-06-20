#!/bin/zsh
if [[ -n "$1" ]] ; then
  filetype=$(file -b "$1")
  filetext=$(print - $filetype | grep -i "text")
  if [[ ${filetext} ]] ; then
    # do not concatenate text files larger than 10M
    if [[ ${$(du -s "$1")[1]} -gt "10000" ]] ; then
      print "(FILE TOO LARGE TO DISPLAY)\n\n$filetext"
    else
      cat "$1"
    fi
  else
    # show `file' information for non-text files not handled in vifmrc
    print - "$filetype"
  fi
else
  print "(NOTHING TO DISPLAY)"
fi
