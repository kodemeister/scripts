#!/bin/bash

echo_color()
{
	echo "$(tput setaf $2)$1$(tput sgr 0)"
}

SCAN_DIR="$1"
if [ -z "$SCAN_DIR" ]; then
	SCAN_DIR="."
fi

echo_color "Scanning directory $SCAN_DIR" 4

find "$SCAN_DIR" -type f -name "*.cue" -print0 | while IFS= read -r -d $'\0' CUE_FILE; do
	echo_color "Found CUE file $CUE_FILE" 4

	if [ -f "${CUE_FILE%.cue}.ape" ]; then
		AUDIO_FILE="${CUE_FILE%.cue}.ape"
	elif [ -f "${CUE_FILE%.cue}.flac" ]; then
		AUDIO_FILE="${CUE_FILE%.cue}.flac"
	elif [ -f "${CUE_FILE%.cue}.wv" ]; then
		AUDIO_FILE="${CUE_FILE%.cue}.wv"
	else
		echo_color "Warning: audio file not found, skipping" 3
		continue
	fi

	echo_color "Found audio file $AUDIO_FILE" 4

	SPLIT_DIR="${CUE_FILE%.cue}.tmp"
	mkdir -p "$SPLIT_DIR"

	if ! shnsplit -d "$SPLIT_DIR" -f "$CUE_FILE" -o "flac flac -V --best -o %f -" -t "%n - %t" "$AUDIO_FILE"; then
		echo_color "Error: failed to split audio file $AUDIO_FILE" 1
		exit 1
	fi

	if ! cuetag.sh "$CUE_FILE" "$SPLIT_DIR"/*.flac; then
		echo_color "Error: failed to tag splitted files" 1
		exit 1
	fi

	rm -f "$SPLIT_DIR/00 - pregap.flac"
	mv "$SPLIT_DIR"/*.flac "$SPLIT_DIR/.."
	rm -rf "$SPLIT_DIR"
	rm "$CUE_FILE" "$AUDIO_FILE"
	echo_color "Successfully splitted audio file $AUDIO_FILE" 2
done
