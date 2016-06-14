#!/bin/sh
# Script to setup additional local file directory

echo "Starting local personal file directory setup"

SETUP_DIR=~/.vim/mydir
UNDO_DIR="$SETUP_DIR/undodir"
HOME_UNDO_DIR=~/.undodir
SNIP_DIR="$SETUP_DIR/mysnips/Ultisnips"

# If mydir, undodir and snip directories are already present, abort
if [ -e "$SETUP_DIR" ] && [ -e "$UNDO_DIR" ] && [ -e "$SNIP_DIR" ]; then
  exit 1
fi

# If setup directory is not present, make it before you carry on:
if ! [ -e "$SETUP_DIR" ]; then
	echo "Creating your local setup directory"
	mkdir -p "$SETUP_DIR"
fi

# Move persistent undotree directory if it already exists:
if ! [ -e "$UNDO_DIR" ] && [ -e "$HOME_UNDO_DIR" ]; then
	echo "Moving your $HOME_UNDO_DIR to $UNDO_DIR"
	mv "$HOME_UNDO_DIR" "$UNDO_DIR"
fi

# Create new persistent undotree directory:
if ! [ -e "$UNDO_DIR" ] && ! [ -e "$HOME_UNDO_DIR" ]; then
	echo "Creating new persistent undotree directory"
	mkdir -p "$UNDO_DIR"
fi

# Create your personal snippet directory:
if ! [ -e "$SNIP_DIR" ]; then
	echo "Creating your local snippet directory"
	mkdir -p "$SNIP_DIR"
fi

echo "Done."
