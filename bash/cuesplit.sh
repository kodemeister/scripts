#!/bin/bash

scan_dir() {
  echo "Scanning directory $1"

  find "$1" -type f -name "*.cue" -print0 | while IFS= read -r -d $'\0' cue_file; do
    split_audio_file "$cue_file"
  done
}

split_audio_file() {
  local -r cue_file="$1"
  echo "Found CUE file $cue_file"

  for extension in ape flac wv; do
    if [[ -f "${cue_file%.cue}.$extension" ]]; then
      local -r audio_file="${cue_file%.cue}.$extension"
    fi
  done

  if [[ -z "$audio_file" ]]; then
    echo "Warning: audio file not found, skipping"
    return
  fi

  echo "Found audio file $audio_file"

  local -r split_dir="${cue_file%.cue}.tmp"
  mkdir -p "$split_dir"

  if ! shnsplit -d "$split_dir" -f "$cue_file" -o "flac flac -V --best -o %f -" -t "%n - %t" "$audio_file"; then
    echo "Error: failed to split audio file $audio_file"
    exit 1
  fi

  if ! cuetag.sh "$cue_file" "$split_dir"/*.flac; then
    echo "Error: failed to tag split files"
    exit 1
  fi

  rm -f "$split_dir/00 - pregap.flac"
  mv "$split_dir"/*.flac "$split_dir/.."
  rm -rf "$split_dir"
  rm "$cue_file" "$audio_file"

  echo "Successfully split audio file $audio_file"
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
