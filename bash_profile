# -*- mode: sh -*-
set -o pipefail
export HISTCONTROL=erasedups
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
export PATH=/Users/milodavis/racket/fork/racket/bin/:$PATH
export PATH=/Users/milodavis/.opam/system/bin/:$PATH
export PATH=/usr/texbin:$PATH
export PATH=/usr/local/texlive/2015/texmf-dist/fonts/tfm:$PATH
export LANG=en_US.UTF-8
export HOMEBREW_GITHUB_API_TOKEN=$(cat ~/config/github_api_token)
export CLASSPATH=/Users/milodavis/classpath/*:$CLASSPATH
alias mv="mv -i"
alias cp="cp -i"
alias ack="ack -i"              # Make ack case insensitive

# export PYTHONPATH=/usr/local/lib/python3.5/site-packages:$PYTHONPATH

GREEN="\[$(tput setaf 2)\]"
RESET="\[$(tput sgr0)\]"
export PS1="\n\[\e[34m\]\w\[\e[m\]\\n$ "

# For laughs/times of extreme boredom
alias star-wars="telnet towel.blinkenlights.nl"

function reload() {
    cwd=$(pwd)
    source ~/.bash_profile
    cd $cwd
}

function search () {
    find . -iname "*$1*";
}

function wstrip () {
	cat ${1-/dev/stdin} | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//';
}
function every () { while True; do $*; sleep 1; done; }
function every-nth () { interval=$1; shift; while True; do $*; sleep $interval; echo; done; }
function repeat () { num_execs="$1"; shift; for i in `seq $num_execs`; do $*; done; }
function alert () { terminal-notifier -message "$1"; }
alias lcase="tr '[A-Z]' '[a-z]'"
switchd () { pushd; }
alias sd="switchd"

# Emacsclient
export EDITOR="emacsclient -a '' -c"
alias emacs="emacsclient -c"

# Directories
export HTDOCS="/Applications/XAMPP/htdocs/"
export TYPED_RACKET="~/racket/fork/racket/share/pkgs/typed-racket-lib/"

# GUI programs
alias intellij="open /Applications/IntelliJ\ IDEA.app/"
alias chrome="open /Applications/Google\ Chrome.app"
alias activity="open /Applications/Utilities/Activity\ Monitor.app"
alias terminal="open /Applications/Utilities/Terminal.app"

# Syntax highlighting utility (use with keynote)
alias highlight="highlight -O rtf "

function hash-wifi-password () {
	echo -n $1 | iconv -t utf16le | openssl md4;
	history -d $((HISTCMD-1));
}


if [[ "$OSTYPE" =~ "*darwin*" ]]; then
    [ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
	if ! pgrep -u $USER ssh-agent > /dev/null; then
		ssh-agent > ~/.ssh-agent-thing
	fi

	if [[ "$SSH_AGENT_PID" == "" ]]; then
		eval $(<~/.ssh-agent-thing);
	fi
fi

if [[ "$OSTYPE" = *"darwin"* ]]; then
	ssh-add -K ~/.ssh/ccis_github 2> /dev/null
	ssh-add -K ~/.ssh/git-ec2 2> /dev/null
	ssh-add -K ~/.ssh/github 2> /dev/null
	ssh-add -K ~/.ssh/gitlab 2> /dev/null
fi

# I somehow keep turning off my xmodmap configuration
# I'm not sure how and haven't sat down to fix it yet
# this hack fixes the issues
function CAPS () {
	setxkbmap; sleep 2; xmodmap ~/.xmodmap;
}

alias start-tezos-node="source ~/notes/init-tezos.sh"
function stop-tezos-nodes () {
    pgrep tezos-node | xargs -r -I{} kill -9 {}
}
function alphanet () {
    ~/tezos/repo/scripts/alphanet.sh "$@"
}

# I like how more automatically scrolls in the emacs shell
# TODO: figure out how to get less to do this
export PAGER=more
export MORE=-R
export LESS=-R

export TZPATH="/Users/milodavis/tezos/repo/"

# This may brake things break outside of M-x shell
# but that's my primary terminal, so whatever
export TERM=xterm-256color

# For easy parallel make
MAKE_THREADS=4
alias makep="make -j ${MAKE_THREADS}"

# OPAM configuration
. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

cd ~
