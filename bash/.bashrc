#
# ~/.bashrc
#

#######################################
#             Functions               #
#######################################

# Parse current git branch
function parse_git_branch()
{
    git branch 2> /dev/null | grep -e '*' | sed -e 's/* \(.*\)/ (\1)/'
}

# Parse git status
function parse_git_status()
{
    if [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]] ; then
        echo -n "*"
    elif [[ $(git status --porcelain 2> /dev/null | grep -e "^??") != "" ]] ; then
        echo -n "+"
    fi
}

# Get time of the last command
function timer_now()
{
    date +%s%N
}

function timer_start()
{
    timer_start=${timer_start:-$(timer_now)}
}

function timer_stop()
{
    local delta_us=$((($(timer_now) - $timer_start) / 1000))
    local us=$((delta_us % 1000))
    local ms=$(((delta_us / 1000) % 1000))
    local s=$(((delta_us / 1000000) % 60))
    local m=$(((delta_us / 60000000) % 60))
    local h=$((delta_us / 3600000000))
    # Goal: always show around 3 digits of accuracy
    if ((h > 0)); then timer_show=${h}h${m}m
    elif ((m > 0)); then timer_show=${m}m${s}s
    elif ((s >= 10)); then timer_show=${s}.$((ms / 100))s
    elif ((s > 0)); then timer_show=${s}.$(printf %03d $ms)s
    elif ((ms >= 100)); then timer_show=${ms}ms
    elif ((ms > 0)); then timer_show=${ms}.$((us / 100))ms
    else timer_show=${us}us
    fi
    unset timer_start
}

#######################################
#               Script                #
#######################################

# Check if running interactively
[[ $- != *i* ]] && return

# Display new terminal message
cat ~/.sholde

# Bash completion
[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Change the window title of X terminals
case ${TERM} in
    xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|interix|konsole*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
        ;;
    screen*)
        PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
        ;;
esac

# Time of the last command
trap 'timer_start' DEBUG
PROMPT_COMMAND=timer_stop

# Test if colorful terminal is used
if [[ ${EUID} == 0 ]] ; then
    PS1='[\u@\h \W]$(parse_git_branch) \$ '
else
    # PS1
    PS1='\[\033[1;34m\][$?] '                                       # error
    PS1+='\[\033[01;32m\][\u@\h \[\033[1;34m\]\W\[\033[01;32m\]]'   # usual prompt
    PS1+='\[\033[1;31m\]$(parse_git_branch)$(parse_git_status) '    # git
    PS1+='\[\033[1;33m\](${timer_show}) '                           # time

    if [ $(id -u) -eq 0 ] ; then
        PS1+='\[\033[1;31m\]#\[\033[00m\] '                         # root
    else
        PS1+='\[\033[01;32m\]\$\[\033[00m\] '                       # user
    fi

    # Color alias
    alias ls='ls --color=auto'
    alias grep='grep --colour=auto'
    alias egrep='egrep --colour=auto'
    alias fgrep='fgrep --colour=auto'
fi

# history length
HISTSIZE=1000
HISTFILESIZE=2000

# Important
xhost +local:root > /dev/null 2>&1

complete -cf sudo

#######################################
#                SHOPT                #
#######################################

shopt -s cdspell
shopt -s cmdhist
shopt -s checkwinsize
shopt -s expand_aliases
shopt -s histappend

#######################################
#                Alias                #
#######################################

if [ -f ~/.bash_aliases ] ; then
    source ~/.bash_aliases
fi

#######################################
#          Export variable            #
#######################################

if [ -f ~/.bash_export ] ; then
    source ~/.bash_export
fi
