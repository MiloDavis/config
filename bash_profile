set -o pipefail
export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
export PATH=/Library/Java/JavaVirtualMachines/jdk1.8.0_25.jdk/bin:$PATH
export PATH=~/tools/maven/bin:$PATH
export PATH=~/tools/srilm/:$PATH
export PATH=/usr/java/local/opt/coreutils/libexec/gnubin:$PATH
export LANG=en_US.UTF-8
export HOMEBREW_GITHUB_API_TOKEN="fba0b603eeeb996ed69ea2f29c52e91021c62f4a"
export JAVA_HOME=`/usr/libexec/java_home -v 1.8.0_25`
unset CLASSPATH
export CLASSPATH=~/tools/stanford-parser-full-2015-04-20/stanford-parser.jar
export CLASSPATH=/usr/local/share/py4j/py4j0.9.1.jar:$CLASSPATH
export CLASSPATH=/Users/milo/tools/mallet/class:/Users/milo/tools/mallet/lib/mallet-deps.jar:$CLASSPATH
export CLASSPATH=/Users/milo/tools/stanford-corenlp/$CLASSPATH
export TCAT_PATH=/Users/milo/tcat/
export TVEC_PATH=/Users/milo/tvec/
export EDITOR=emacs
alias mv="mv -i"
alias cp="cp -i"

source ~/.git-prompt.sh
PROMPT_COMMAND="echo"
# Old prompt '\[\033[32m\]\w$(__git_ps1 " (%s)"):\[\033[00m\]\n$ '
BLUE='\[\033[34m\]'
DARK_GREY='\[\033[1;30m\]'
DARK_RED='\[\033[0;124m\]'
export PS1=$BLUE'\w$(__git_ps1 " (%s)"):\[\033[0m\]\n$ '

alias reload="source ~/.bash_profile"
alias activatedle="source /Users/milo/deep-learning-experiments/env/bin/activate"
alias ciderpress="ssh ciderpress.basistech.net"
function search () { 
    find . -iname *$1*;
}

export ROSETTE_API="d7e42e7f9c54a653ebb0736bdb5bcd67"

# This probably isn't current
export ec2=ec2-54-88-31-67.compute-1.amazonaws.com

alias sshec2="ssh -i ~/deep-learning-experiments/convnet/DLEKey.pem ubuntu@$ec2"

export JVM_ARGS=" -Xmx6g "
export PYTHONPATH=~/tcat/script:$PYTHON_PATH
# export PYTHONPATH=~/tvec/python/:$PYTHON_PATH
function tokenize-help () {
    /Users/milo/flinx/core/rbl-je-7.14.500.c57.2/tools/bin/RBLCmd annotate \
                    --rootDirectory=/Users/milo/flinx/core/rbl-je-7.14.500.c57.2 \
		    --language eng -showRawResults --tokenizeForScript true -i ${1-/dev/stdin};
}
function pos-tag-help () {
    /Users/milo/flinx/core/rbl-je-7.14.500.c57.2/tools/bin/RBLCmd analyze -i ${1-/dev/stdin} \
    --rootDirectory=/Users/milo/flinx/core/rbl-je-7.14.500.c57.2 \
		   --language eng --disambiguate true -showTokenDetails
}
function stdin-wrapper () {
    input=${1-/dev/stdin}
    if [ -a "$input" ]; then
	$2 $input;
    else
	echo $1 | $2 /dev/stdin
    fi
}
function tokenize () { stdin-wrapper "$1" tokenize-help; }
function pos-tag () { stdin-wrapper "$1" pos-tag-help; }
function wstrip () { 
    cat ${1-/dev/stdin} | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//';
}
function every () { while True; do $*; sleep 1; done; }
function every-nth () { interval=$1; shift; while True; do $*; sleep $interval; echo; done; }
alias scoresent="pushd ~/tcat/script; ./score-sentiment.sh /tmp; popd"
alias lcase="tr '[A-Z]' '[a-z]'"
switchd () { pushd; }

function stanford-tokenize () {
    cat ${1-/dev/stdin} | java edu.stanford.nlp.process.PTBTokenizer 2> /dev/null;
}

# export BT_ROOT=~/.m2/repository/bt/jug/$(ls -1 ~/.m2/repository/bt/jug/ | tail -1)
alias t5sort="$BT_ROOT/rlp/bin/*/t5sort"
alias t5build="$BT_ROOT/rlp/bin/*/t5build -5 -h 10000000 "
alias rbl-docs="open ~/tools/doc/rbl/doc/apidocs/rbl-je/com/basistech/rosette/package-summary.html"
dicts=~/truecaser/saved/ngram-dictionaries
function truecase () {
    input=${1-/dev/stdin}
    shift 1
    if [ -a "$input" ]; then
		~/truecaser/script/run-java.sh com.basistech.rosette.truecaser.TrueCaser $dicts "$input" $*
    else
		echo "$input" | ~/truecaser/script/run-java.sh com.basistech.rosette.truecaser.TrueCaser $dicts /dev/stdin $*
    fi
}


if [ $EMACS ]; then
    alias clear="yes '' | head -`tput lines`"
fi


alias intellij="open /Applications/IntelliJ\ IDEA\ 15\ CE.app/"
alias mv="mv -i"
