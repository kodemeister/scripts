#!/bin/bash

echo_color()
{
  echo "$(tput setaf $2)$1$(tput sgr 0)"
}

split_audio_file()
{
  local cue_file="$1"
  echo_color "Found CUE file $cue_file" 4

  if [[ -f "${cue_file%.cue}.ape" ]]; then
    local audio_file="${cue_file%.cue}.ape"
  elif [[ -f "${cue_file%.cue}.flac" ]]; then
    local audio_file="${cue_file%.cue}.flac"
  elif [[ -f "${cue_file%.cue}.wv" ]]; then
    local audio_file="${cue_file%.cue}.wv"
  else
    echo_color "Warning: audio file not found, skipping" 3
    return
  fi

  echo_color "Found audio file $audio_file" 4

  local split_dir="${cue_file%.cue}.tmp"
  mkdir -p "$split_dir"

  if ! shnsplit -d "$split_dir" -f "$cue_file" -o "flac flac -V --best -o %f -" -t "%n - %t" "$audio_file"; then
    echo_color "Error: failed to split audio file $audio_file" 1
    exit 1
  fi

  if ! cuetag.sh "$cue_file" "$split_dir"/*.flac; then
    echo_color "Error: failed to tag splitted files" 1
    exit 1
  fi

  rm -f "$split_dir/00 - pregap.flac"
  mv "$split_dir"/*.flac "$split_dir/.."
  rm -rf "$split_dir"
  rm "$cue_file" "$audio_file"

  echo_color "Successfully splitted audio file $audio_file" 2
}

main()
{
  local scan_dir="$1"
  if [[ -z "$scan_dir" ]]; then
    scan_dir="."
  fi

  echo_color "Scanning directory $scan_dir" 4

  find "$scan_dir" -type f -name "*.cue" -print0 | while IFS= read -r -d $'\0' cue_file; do
    split_audio_file "$cue_file"
  done
}

main "$@"
