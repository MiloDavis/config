# -*- mode: sh -*-
set -o pipefail
alias emacs="emacsclient -c"
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
export PATH=/usr/java/local/opt/coreutils/libexec/gnubin:$PATH
export LANG=en_US.UTF-8
export HOMEBREW_GITHUB_API_TOKEN="fba0b603eeeb996ed69ea2f29c52e91021c62f4a"
export EDITOR="emacsclient -a '' -c"
alias mv="mv -i"
alias cp="cp -i"
alias ctags='/usr/local/bin/ctags'

GREEN="\[$(tput setaf 2)\]"
RESET="\[$(tput sgr0)\]"
export PS1="\n\[\e[34m\]\w\[\e[m\]\\n$ "

alias rdoc="open /Users/milodavis/racket/fork/racket/doc/help/index.html"
alias racket="racket -l errortrace -t "
alias star-wars="telnet towel.blinkenlights.nl"
alias greps="grep -rn . -e "
export HTDOCS=/Applications/XAMPP/htdocs/
alias ssh-ec2="ssh -i ~/Documents/milodaviscom.pem ubuntu@milodavis.com"


alias reload="source ~/.bash_profile"
function search () { 
    find . -iname *$1*;
}

function stdin-wrapper () {
    input=${1-/dev/stdin}
    if [ -a "$input" ]; then
	$2 $input;
    else
	echo $1 | $2 /dev/stdin
    fi
}
function wstrip () { 
    cat ${1-/dev/stdin} | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//';
}
function every () { while True; do $*; sleep 1; done; }
function every-nth () { interval=$1; shift; while True; do $*; sleep $interval; echo; done; }
alias lcase="tr '[A-Z]' '[a-z]'"
switchd () { pushd; }

# GUI programs
alias intellij="open /Applications/IntelliJ\ IDEA\ 15\ CE.app/"
alias mv="mv -i"
alias chrome="open /Applications/Google\ Chrome.app"
