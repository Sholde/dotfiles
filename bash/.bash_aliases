#######################################
#                Alias                #
#######################################

# Sudo (to enable alias when using sudo command)
alias sudo="sudo"

# Reload configuration
alias reload="source ~/.bashrc"

# Default
alias df="df -h"                          # human-readable sizes
alias free="free -ht"                     # show sizes in MB
alias more="less"                         # less is better
alias np="nano -w PKGBUILD"

# Color alias
alias ls="ls --color=auto"
alias grep="grep --colour=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"
alias diff="diff --color=auto"
alias pacman="pacman --color=auto"

# pacman
alias update="pacman -Syu"
alias refresh="pacman -Syyu"
alias unlock="sudo rm /var/lib/pacman/db.lck"

# get fastest mirrors
alias mirror="reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
alias mirrord="reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
alias mirrors="reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
alias mirrora="reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

# work
alias cdwork="cd ~/Documents"

# list
alias l="ls"
alias ll="ls -lh"
alias la="ls -a"
alias lla="ls -lha"
alias lsc="ls --format=single-column"
alias lsca="ls --format=single-column -a"
alias l.="ls -a | grep -E '^\.'"
## octal permission
ols() { stat -c "%A %a %n" ${@} ; }

# clean
alias clean="rm -Rf *~ .*~"
alias c="clear"

# confirm before owerwriting
alias cp="cp -iv"
alias mv="mv -iv"

# mode
alias mode="namei -l"

# mkdir
alias mkdir="mkdir -vp"

# cd
alias back="cd -"
alias cd..="cd .."
alias ..="cd .."
parent() { for i in $(seq 1 ${1}); do cd .. ; done ; }

# path
printpath() { echo ${1} | sed 's/:/\n/g' | sed '/^\s*$/d' ; }
path() { printpath ${PATH} ; }
ldpath() { printpath ${LD_LIBRARY_PATH} ; }
libpath() { printpath ${LIBRARY_PATH} ; }
cpath() { printpath ${CPATH} ; }
manpath() { printpath ${MANPATH} ; }

# env
alias omp="env | grep ^OMP_"
alias mpi="env | grep ^MPI_"
alias slu="env | grep ^SLURM_"
alias grenv="env | grep"

# make (avoid mistakes)
alias maek="echo 'Try make'"
alias mkae="echo 'Try make'"
alias mkea="echo 'Try make'"

# du
alias duhs="du -hs *"

# ps
alias psa="ps auxf"
alias psgrep="ps aux | grep -v grep | grep -i -e VSZ -e"
alias psmem='ps auxf | sort -nr -k 4'
alias pscpu='ps auxf | sort -nr -k 3'


# emacs
alias emacs="emacs -nw"

# avoid prank
mesg n
alias vi="emacs -nw"
alias vim="emacs -nw"
alias nvim="emacs -nw"
alias sl="emacs -nw"

# Find alias
alias ff="find . -type f -iname"
alias fsafe="find . -type f ${@} 2> >(grep -v Permission)"

# locate command
locate() { find / -name ${1} 2> >(grep -v Permission) ; }

# search expression in all file here
search() { grep -n -re "${1}" * ; }

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
alias os-release="cat /etc/os-release"
os() { cat /etc/os-release | grep PRETTY_NAME | awk -F'"' '{print $2}' ; }

# mount
alias mnt="mount | awk -F' ' '{ printf \"%s\t%s\n\",\$1,\$3; }' | column -t | grep -E ^/dev/ | sort"

# poweroff
alias off="shutdown now"

# ip
## Global ip
alias gip="curl https://ifconfig.co"
## Local ip
lip() { ip addr | grep inet | sed -n 2p | awk '{print $2}' | cut -d'/' -f1 ; }

# lock screen
alias lock="i3lock -i ~/Pictures/wallpapers/background/default.png"

# tar
alias tgz="tar -czf"
alias untar='tar -xzf'

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
## usage: tarball <directory|file>
tarball()
{
    if [ -d $1 ] || [ -f $1 ] ; then
        tar -czf $1.tar.gz $1
    else
        echo "'$1' in not a valid directory or file"
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
            *.deb)       ar x $1      ;;
            *.tar.xz)    tar xf $1    ;;
            *.tar.zst)   unzstd $1    ;;
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

# Box print
#blankline() { echo "" ; }
boxprint()
{
    if [ ${#} -ne 1 ] ; then
        echo "Error: expected string as argument"
        return 1
    fi
    string=$1
    hashtagline=$(echo -n "##" ; for i in `seq 2 $(echo "${string}" | wc -c)` ;  do echo -n '#' ; done ; echo "##")
    stringwithhashtag=$(echo -n "# " ; echo -n ${string} ; echo " #")
    blankline=$(echo "")

    echo ${blankline}
    echo ${hashtagline}
    echo ${stringwithhashtag}
    echo ${hashtagline}
    echo ${blankline}
}
