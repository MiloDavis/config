#!/bin/bash

function relink () {
	file=$(basename $1)
	rm -f ~/.$file
	ln -s `pwd`/$file ~/.$file
}

export -f relink

find . -type f -maxdepth 1 | grep -v ".sh" | parallel -I {} relink {}
