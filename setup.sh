#!/bin/bash
find . -type f -maxdepth 1 | xargs -I {} basename {} | xargs -I {} ln -s `pwd`/{} ~/.{}
# ln -s `pwd`/emacs ~/.emacs
# ln -s `pwd`/bash_profile ~/.bash_profile
# ln -s `pwd`/bashrc ~/.bashrc
# ln -s `pwd`/bashrc ~/.bashrc
