#
# ~/.bashrc
#

# Check if running interactively
[[ $- != *i* ]] && return

# Display new terminal message
figlet sholde
neofetch

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

# Parse current git branch
parse_git_branch()
{
    git branch 2> /dev/null | grep -e '*' | sed -e 's/* \(.*\)/ (\1)/'
}

# Test if colorful terminal is used
if [[ ${EUID} == 0 ]] ; then
    PS1='[\u@\h \W]$(parse_git_branch) \$ '
else
    # PS1
    PS1='\[\033[1;34m\][$?] \[\033[01;32m\][\u@\h \[\033[1;34m\]\W\[\033[01;32m\]]\[\033[1;31m\]$(parse_git_branch) \[\033[01;32m\]\$\[\033[00m\] '

    # Color alias
    alias ls='ls --color=auto'
    alias grep='grep --colour=auto'
    alias egrep='egrep --colour=auto'
    alias fgrep='fgrep --colour=auto'
fi

#
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias more="less"                         # less is better
alias np='nano -w PKGBUILD'

# work
alias cdmaster="cd ~/dev/master"
alias cdaur="cd ~/dev/aur"
alias cdperso="cd ~/dev/perso"
alias cdverificarlo="cd ~/dev/master/projet/verificarlo"

# list
alias ll="ls -l"
alias la="ls -a"
alias lla="ls -la"

# clean
alias cl="clear"
alias clean="rm -Rf *~ .*~"

# mkdir
alias mcd="mkdir -p $1 && cd $1"
alias mkdir="mkdir -p"

# emacs
alias emacs="emacs -nw"

# Find here
fhere() {
    find . -name "$@"
}

# Find in user space
fuser() {
    find ~/ -name "$@"
}

# Doesn't print Permission denied file
fsafe() {
    find "$@" 2> >(grep -v "Permission")
}

# locate command
locate() {
    find / -name "$1" 2> >(grep -v "Permission")
}

# translate fr to en
transen() {
    trans -s fr -t en "$@"
}

# translate en to fr
transfr() {
    trans -s en -t fr "$@"
}

# search expression in all file here
search() {
    grep -re "$1" *
}

# UI
alias tbmail="thunderbird 2> /dev/null &"
alias discord="discord 2> /dev/null &"
alias evince="evince 2> /dev/null "
alias scilab="~/Téléchargements/scilab-6.1.0/bin/scilab 2>/dev/null &"

#
xhost +local:root > /dev/null 2>&1

complete -cf sudo

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize

shopt -s expand_aliases

# Enable history appending instead of overwriting.  #139609
shopt -s histappend

#
# # ex - archive extractor
# # usage: ex <file>
ex ()
{
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1   ;;
            *.tar.gz)    tar xzf $1   ;;
            *.bz2)       bunzip2 $1   ;;
            *.rar)       unrar x $1     ;;
            *.gz)        gunzip $1    ;;
            *.tar)       tar xf $1    ;;
            *.tbz2)      tar xjf $1   ;;
            *.tgz)       tar xzf $1   ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1;;
            *.7z)        7z x $1      ;;
            *)           echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# export

# # man and less color
export LESS_TERMCAP_md=$'\e[01;31m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_us=$'\e[01;32m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_so=$'\e[45;93m'
export LESS_TERMCAP_se=$'\e[0m'

# # other
export EDITOR="emacs -nw"

# # PATH
if ! echo $PATH | grep -q /opt/spack/bin ; then
    export PATH=$PATH:/opt/spack/bin
fi

# # # MPICH
mpich_path=/usr/local/mpich-3.4.1/bin

# Load MPICH
loadmpich()
{
    # Case load
    if echo $PATH | grep -q $mpich_path ; then
        echo "MPICH is already load!"
    # Case is not load
    else
        export PATH=$PATH:$mpich_path
        echo "MPICH load!"
    fi
}

# Unload MPICH
# Use + on sed because we use path, therefore we have slash (/) on our expresion
# and it not match
unloadmpich()
{
    # Case :path:
    if echo $PATH | grep -q ":"$mpich_path":" ; then
        export PATH=$(echo $PATH | sed -e "s+:$mpich_path:+:+g")
        echo "MPICH unload!"
    # Case :path
    elif echo $PATH | grep -q ":"$mpich_path ; then
        export PATH=$(echo $PATH | sed -e "s+:$mpich_path++g")
        echo "MPICH unload!"
    # Case path:
    elif echo $PATH | grep -q $mpich_path":" ; then
        export PATH=$(echo $PATH | sed -e "s+$mpich_path:++g")
        echo "MPICH unload!"
    # Case path
    elif echo $PATH | grep -q $mpich_path ; then
        export PATH=$(echo $PATH | sed -e "s+$mpich_path++g")
        echo "MPICH unload!"
    # Case is not load
    else
        echo "MPICH is NOT load!"
    fi
}

# # Verificarlo
export PYTHONPATH="/usr/local/lib/python3.8/site-packages"
export VFC_BACKENDS="libinterflop_ieee.so"

setmpicarlo()
{
    export OMPI_CC="verificarlo-c"
    export OMPI_CXX="verificarlo-c++"
    export OMPI_F="verificarlo-f"
}

unsetmpicarlo()
{
    unset OMPI_CC
    unset OMPI_CXX
    unset OMPI_F
}
