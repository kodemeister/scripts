#!/bin/bash

declare -r BLACK=0
declare -r RED=1
declare -r GREEN=2
declare -r YELLOW=3
declare -r BLUE=4
declare -r MAGENTA=5
declare -r CYAN=6
declare -r WHITE=7

scan_dir() {
  print_message "Scanning directory $1" $BLUE

  find "$1" -type f -name "*.cue" -print0 | while IFS= read -r -d $'\0' cue_file; do
    split_audio_file "$cue_file"
  done
}

split_audio_file() {
  local -r cue_file="$1"
  print_message "Found CUE file $cue_file" $BLUE

  if [[ -f "${cue_file%.cue}.ape" ]]; then
    local -r audio_file="${cue_file%.cue}.ape"
  elif [[ -f "${cue_file%.cue}.flac" ]]; then
    local -r audio_file="${cue_file%.cue}.flac"
  elif [[ -f "${cue_file%.cue}.wv" ]]; then
    local -r audio_file="${cue_file%.cue}.wv"
  else
    print_message "Warning: audio file not found, skipping" $YELLOW
    return
  fi

  print_message "Found audio file $audio_file" $BLUE

  local -r split_dir="${cue_file%.cue}.tmp"
  mkdir -p "$split_dir"

  if ! shnsplit -d "$split_dir" -f "$cue_file" -o "flac flac -V --best -o %f -" -t "%n - %t" "$audio_file"; then
    print_message "Error: failed to split audio file $audio_file" $RED
    exit 1
  fi

  if ! cuetag.sh "$cue_file" "$split_dir"/*.flac; then
    print_message "Error: failed to tag splitted files" $RED
    exit 1
  fi

  rm -f "$split_dir/00 - pregap.flac"
  mv "$split_dir"/*.flac "$split_dir/.."
  rm -rf "$split_dir"
  rm "$cue_file" "$audio_file"

  print_message "Successfully splitted audio file $audio_file" $GREEN
}

print_message() {
  echo "$(tput setaf $2)$1$(tput sgr 0)"
}

main() {
  if [[ $# -eq 0 ]]; then
    set -- "."
  fi

  for dir in "$@"; do
    scan_dir "$dir"
  done
}

main "$@"
