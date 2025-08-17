#!/bin/zsh
gputemp=$(nvidia-smi --format=nounits,csv,noheader --query-gpu=temperature.gpu | xargs echo)
if [[ "$(echo $gputemp | cut -d ' ' -f 1)" == "Failed" ]] ; then
  echo " %{F-}read  RELOAD DRIVER"
  return 1
fi
if [[ "$gputemp" -ge 65 && "$gputemp" -lt 80 ]] ; then
  echo "  %{F#EAE900}%{F-} $gputemp°C"
elif [[ "$gputemp" -ge 80 ]] ; then
  echo "  %{F#C2003B}%{F-} $gputemp°C"
else
  echo "  %{F#616161}%{F-} $gputemp°C"
fi
