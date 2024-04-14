#
# ~/.bashrc
#

#######################################
#             Functions               #
#######################################

# Parse current git branch
function parse_git_branch()
{
    git branch 2> /dev/null | grep -e '*' | sed -e 's/* \(.*\)/(\1)/'
}

# Parse git status
function parse_git_status()
{
    if [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]] ; then
        echo -n "*"
    elif [[ $(git status --porcelain 2> /dev/null | grep -e "^??") != "" ]] ; then
        echo -n "+"
    fi
    if [ $(git branch 2> /dev/null | wc -l) -ne 0 ] ; then
        echo -n " "
    fi
}

# Get time of the last command
function timer_now()
{
    date +%s%N
}

# Timer functions
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

# Parse last error code
function error_code()
{
    last_error=$(echo $?)
    BLUE="\033[1;34m"
    RED="\033[1;31m"
    if [ ${last_error} -eq 0 ] ; then
        echo -ne "${BLUE}[${last_error}]"
    else
        echo -ne "${RED}[${last_error}]"
    fi
}

# Find if there is background jobs
function count_jobs()
{
    PURPLE="\033[1;35m"
    count=$(jobs | wc -l)
    if [ ${count} -ne 0 ] ; then
        echo -ne "${PURPLE}{${count} jobs}"
    fi
}

#######################################
#               Script                #
#######################################

# Display new terminal message
if [ "$(hostname)" == "nitro" ] ; then
    cat ~/.profile_nitro
elif [ "$(hostname)" == "nrv" ] ; then
    cat ~/.profile_nrv
else
    cat ~/.sholde
fi

# Time of the last command
trap 'timer_start' DEBUG
PROMPT_COMMAND="timer_stop"

YELLOW="\[\033[1;33m\]"
GREEN="\[\033[01;32m\]"
BLUE="\[\033[1;34m\]"
RED="\[\033[1;31m\]"
WHITE="\[\033[00m\]"
# PS1
PS1='$(error_code) '                                   # error
PS1+="${YELLOW}"'$(date +%H:%M:%S) '                   # time
PS1+="${GREEN}[\u@\h ${BLUE}\W${GREEN}] "              # usual prompt
PS1+="${YELLOW}"'(${timer_show}) '                     # delay
PS1+="${RED}"'$(parse_git_branch)$(parse_git_status)'  # git
PS1+='$(count_jobs)'                                   # jobs
PS1+="\n"
if [ $(id -u) -eq 0 ] ; then                           # root
    PS1+="${RED}#${WHITE} "
    PS2="${RED}#${WHITE} "
else                                                   # user
    PS1+="${GREEN}->${WHITE} "
    PS2="${GREEN}->${WHITE} "
fi

# Important
xhost +local:root > /dev/null 2>&1

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
