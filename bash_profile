# -*- mode: sh -*-
set -o pipefail
export HISTCONTROL=erasedups
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
export PATH=/Users/milodavis/racket/fork/racket/bin/:$PATH
export PATH=/usr/local/lib/python2.7/site-packages:$PATH
export PATH=/Users/milodavis/.opam/system/bin/:$PATH
export PATH=/usr/texbin:$PATH
export PATH=/usr/local/texlive/2015/texmf-dist/fonts/tfm:$PATH
export PATH=~/.userspace-programs:$PATH
export LANG=en_US.UTF-8
export CLASSPATH=/Users/milodavis/classpath/*:$CLASSPATH
alias mv="mv -i"
alias cp="cp -i"
alias ack="ack -i"              # Make ack case insensitive

work_config_file=~/work_local/bash
if [ -f $work_config_file ]; then
    source $work_config_file
fi

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
    search_str="*$1*"
    shift
    find . -iname "$search_str" "$@" | grep -v '\.git' | grep -v '_build';
}

function wstrip () {
    cat ${1-/dev/stdin} | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//';
}
function every () { while True; do $*; sleep 1; done; }
function every-nth () { interval=$1; shift; while True; do $*; sleep $interval; echo; done; }
function repeat () { num_execs="$1"; shift; for i in `seq $num_execs`; do $*; done; }
function alert () {
    if [ -z terminal-notifier ]; then
        terminal-notifier -message "$1";
    else
        notify-send "$1"
    fi
}
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

    export HOMEBREW_GITHUB_API_TOKEN=$(cat ~/config/github_api_token)
    export HOMEBREW_NO_AUTO_UPDATE=1

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
    pgrep -f tezos-node | xargs -r kill -9;
    pgrep -f launch-sandbox-nodes | xargs -r kill -9
    export is_tezos_sandboxed_init=0
}

function alphanet () {
    ~/tezos/repo/scripts/alphanet.sh "$@"
}

# I like how more automatically scrolls in the emacs shell
# TODO: figure out how to get less to do this
export PAGER=more

export TZPATH="/Users/milodavis/tezos/repo/"

# This may brake things break outside of M-x shell
# but that's my primary terminal, so whatever
export TERM=xterm-256color

# For easy parallel make
MAKE_THREADS=`nproc`
alias makep="make -j ${MAKE_THREADS}"

# Prints jbuilder tests in tezos live
export DEV=" --dev --no-buffer"
export REPO_PATH="/home/milo/tezos/repo/"

alias make="make --no-print-directory"
export show_logs=false

alias betanet="TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=yes ~/tezos/repo/tezos-client --addr milodavis.com"

# OPAM configuration
. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

cd ~
