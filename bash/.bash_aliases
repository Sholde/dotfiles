#######################################
#                Alias                #
#######################################

# Sudo (to enable alias when using sudo command)
alias sudo="sudo "

# Default
alias df="df -h"                          # human-readable sizes
alias free="free -m"                      # show sizes in MB
alias more="less"                         # less is better
alias np="nano -w PKGBUILD"

# Color alias
alias ls="ls --color=auto"
alias grep="grep --colour=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"
alias diff="diff --color=auto"
alias pacman="pacman --color=auto"

# work
alias cdwork="cd ~/Documents"

# list
alias l="ls"
alias ll="ls -lh"
alias la="ls -a"
alias lla="ls -lha"
alias lsc="ls --format=single-column"
alias lsca="ls --format=single-column -a"
alias lt="ls -lh -S -1"
alias left="ls -lh -t -1"
## octal permission
ols() { stat -c "%A %a %n" ${@} ; }

# clean
alias clean="rm -Rf *~ .*~"
alias c="clear"

# confirm before owerwriting
alias cp="cp -iv"
alias mv="mv -riv"

# mkdir
alias mkdir="mkdir -vp"

# cd
alias ..="cd .."
alias .2="cd ../../"
alias .3="cd ../../../"
alias .4="cd ../../../../"
alias .5="cd ../../../../.."

# path
alias path="echo -e ${PATH//:/\\n}"

# pacman
alias update="sudo pacman -Syu"
alias updatey="sudo pacman -Syu -noconfirm"

# top
alias topmem="ps auxf | sort -nr -k 4 | head -1"
alias topcpu="ps auxf | sort -nr -k 3 | head -1"

# emacs
alias emacs="emacs -nw"

# Find alias
alias ff="find . -type f -iname"
alias fsafe="find . -type f ${@} 2> >(grep -v Permission)"

# locate command
locate() { find / -name ${1} 2> >(grep -v Permission) ; }

# search expression in all file here
search() { grep -n -re "${1}" * ; }

# translate
## translate fr to en
tren() { trans -s fr -t en "${@}" ; }
## translate en to fr
trfr() { trans -s en -t fr "${@}" ; }

# count
## count word in file
cw() { cat ${1} | wc -w ; }
## count line in file
cl() { cat ${1} | wc -l ; }
## count file in the directory
alias count="find . -type f | wc -l"

# info
alias cpuinfo="cat /proc/cpuinfo"
alias meminfo="cat /proc/meminfo"
alias zoneinfo="cat /proc/zoneinfo"

# mount
alias mnt="mount | awk -F' ' '{ printf \"%s\t%s\n\",\$1,\$3; }' | column -t | egrep ^/dev/ | sort"

# poweroff
alias off="shutdown now"

# ip
## Global ip
alias gip="curl https://ifconfig.co"
## Local ip
lip() { ip addr | grep inet | sed -n 2p | awk '{print $2}' | cut -d'/' -f1 ; }

# UI
alias tbmail="thunderbird 2> /dev/null &"
alias discord="discord 2> /dev/null &"
alias evince="evince ${1} 2> /dev/null"
alias nomacs="nomacs ${1} 2> /dev/null"

# lock screen
alias lock="i3lock -c 000000"

# tar
alias tgz="tar -czvf"
alias untar='tar -xvf'

# Debug mpi program
mpidebug() { mpirun -np $1 xfce4-terminal -e "gdb $2" ; }

# Copy line in file
copy()
{
    # Regular expression for integer
    re='^[0-9]*$'

    # Check number of argument
    if  [ $# == 1 ] ; then

        # Check if $1 is a file
        if [ ! -f $1 ] ; then
            echo "Error: $1 is not a valid file."
            return 1
        fi

        # Real command
        cat $1 | xclip -sel clip

    elif  [ $# == 2 ] ; then

        # Check if $1 is a file
        if [ ! -f $1 ] ; then
            echo "Error: $1 is not a valid file."
            return 1
        fi

        # Check if $2 is a number
        if ! [[ $2 =~ $re ]] ; then
            echo "Usage: copy [FILE] [LINE]"
            echo "Error: LINE must be a number."
            return 1
        fi

        # Real command
        sed -n $2"p" $1 | xclip -sel clip

    elif  [ $# == 3 ] ; then

        # Check if $1 is a file
        if [ ! -f $1 ] ; then
            echo "Error: $1 is not a valid file."
            return 1
        fi

        # Check if $2 and $3 are number and $2 < $3
        if ! [[ $2 =~ $re ]] || ! [[ $3 =~ $re ]] || ! [[ $2 < $3 ]] ; then
            echo "Usage: copy [FILE] [FIRST LINE] [LAST LINE]"
            echo "Error: FIRST LINE and LAST LINE must be numbers and FIRST LINE < LAST LINE."
            return 1
        fi

        # Real command
        sed -n $2,$3"p" $1 | xclip -sel clip

    else

        echo "Usage: copy [FILE] [FIRST LINE] [LAST LINE] (block of line)"
        echo "       copy [FILE] [LINE]                   (single line)"
        return 1

    fi
}

#
## tarball - archive generator
### usage: tarball <file> <directory>
#### note: file is the name without suffix ".tar.gz"
tarball()
{
    if [ -d $2 ] ; then
        cp -R $2 $1
        tar -czvf $1.tar.gz $1
        rm -Rf $1
    else
        echo "'$2' in not a valid directory"
    fi
}

#
## ex - archive extractor
## usage: ex <file>
ex()
{
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1   ;;
            *.tar.gz)    tar xzf $1   ;;
            *.bz2)       bunzip2 $1   ;;
            *.rar)       unrar x $1   ;;
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

# Extract C functions from file
ecfunc()
{
    cat ${1} | grep -P "^[a-zA-Z_]+[[:space:]]*[\*]?[a-zA-Z0-9_-]+\("
}

# Snippet
cmain()
{
    cat > main.c <<EOF
int main(int argc, char **argv)
{
  return 0;
}
EOF
}

mpimain()
{
    cat > main.c <<EOF
#include <mpi.h>

int main(int argc, char **argv)
{
  MPI_Init(&argc, &argv);

  MPI_Finalize();
  return 0;
}
EOF
}

cfile()
{
    # Code file
    cat > ${1}.c <<EOF
#include "${1}.h"
EOF

    # Header file
    upper_header=$(echo ${1} | tr [:lower:] [:upper:])
    cat > ${1}.h <<EOF
#ifndef _${upper_header}_H_
#define _${upper_header}_H_

#endif // _${upper_header}_H_
EOF
}

ccfile()
{
    # Code file
    cat > ${1}.cc <<EOF
#include "${1}.hh"
EOF

    # Header file
    upper_header=$(echo ${1} | tr [:lower:] [:upper:])
    cat > ${1}.hh <<EOF
#ifndef _${upper_header}_HH_
#define _${upper_header}_HH_

#endif // _${upper_header}_HH_
EOF
}
