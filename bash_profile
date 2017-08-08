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
alias ctags='/usr/local/bin/ctags'

# export PYTHONPATH=/usr/local/lib/python3.5/site-packages:$PYTHONPATH

GREEN="\[$(tput setaf 2)\]"
RESET="\[$(tput sgr0)\]"
export PS1="\n\[\e[34m\]\w\[\e[m\]\\n$ "

alias star-wars="telnet towel.blinkenlights.nl"

alias reload="source ~/.bash_profile"

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

function lc-dir () {

	find . -type f ! -executable | xargs -I{} wc -l {} | cut -d' ' -f1 | paste -sd+ | bc
}

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
# To exit screen C-\ C-d on punge
# JS Type Analysis
alias highlight="highlight -O rtf "

function hash-wifi-password () {
	echo -n $1 | iconv -t utf16le | openssl md4;
	history -d $((HISTCMD-1));
}


if [[ "$OSTYPE" =~ "*darwin*" ]]; then

	if ! pgrep -u $USER ssh-agent > /dev/null; then
		ssh-agent > ~/.ssh-agent-thing
	fi

	if [[ "$SSH_AGENT_PID" == "" ]]; then
		eval $(<~/.ssh-agent-thing);
	fi
fi

if [[ "$OSTYPE" = *"darwin"* ]]; then
	ssh-add ~/.ssh/ccis_github 2> /dev/null
	ssh-add ~/.ssh/git-ec2 2> /dev/null
	ssh-add ~/.ssh/github 2> /dev/null
	ssh-add ~/.ssh/gitlab 2> /dev/null
fi

# I somehow keep turning off my xmodmap configuration
# I'm not sure how and haven't sat down to fix it yet
# this function will let me temporarily fix things
function CAPS () {
	setxkbmap; sleep 2; xmodmap ~/.xmodmap;
}

alias start-tezos-node="source ~/notes/init-tezos.sh"
function alphanet () {
    ~/tezos/repo/scripts/alphanet.sh "$@"
}


# OPAM configuration
. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
