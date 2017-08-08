#!/bin/bash

function relink () {
	file=$(basename $1)
	rm -f ~/.$file
	ln -s `pwd`/$file ~/.$file
}

export -f relink

find . -maxdepth 1 -type f | grep -v ".sh" | parallel -I {} relink {}
