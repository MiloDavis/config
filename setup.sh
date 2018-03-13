#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"

for file in `find . -maxdepth 1 -type f -not -name "*.sh"`; do
    name=$(basename $file)
    source=$SCRIPT_DIR/$name
    dest=~/.$name
    echo "Linking $source ---> $dest"
	rm -f ~/.$name
	ln -s $source $dest
done
