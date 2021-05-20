#######################################
#                Alias                #
#######################################

# Default
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias more="less"                         # less is better
alias np='nano -w PKGBUILD'

# work
alias cdmaster="cd ~/dev/master"
alias cdm1="cd ~/dev/master/M1"
alias cdm2="cd ~/dev/master/M2"
alias cdaur="cd ~/dev/aur"
alias cdperso="cd ~/dev/perso"
alias cdvfc="cd ~/dev/master/M1/projet/verificarlo"

# list
alias ll="ls -l"
alias la="ls -a"
alias lla="ls -la"
alias lsc="ls --format=single-column"
alias lsca="ls --format=single-column -a"

# clean
alias clean="rm -Rf *~ .*~"

# confirm before owerwriting
alias cp="cp -i"
alias mv="mv -i"

# mkdir
alias mkdir="mkdir -p"

# pacman
alias psync="sudo pacman -Sy"
alias update="sudo pacman -Syu"

# emacs
alias emacs="emacs -nw"

# Find alias
fhere() { find . -name ${@} ; }
fuser() { find ~/ -name ${@} ; }
fsafe() { find ${@} 2> >(grep -v Permission) ; }

# locate command
locate() { find / -name ${1} 2> >(grep -v Permission) ; }

# translate fr to en
tren() { trans -s fr -t en "${@}" ; }
    
# translate en to fr
trfr() { trans -s en -t fr "${@}" ; }

# search expression in all file here
search() { grep -re "${1}" * ; }

# count word in file
cw() { cat ${1} | wc -w ; }

# count line in file
cl() { cat ${1} | wc -l ; }

# count all in file
ca() { cat ${1} | wc ; }

# info
alias cpuinfo="cat /proc/cpuinfo"
alias meminfo="cat /proc/meminfo"
alias zoneinfo="cat /proc/zoneinfo"

# poweroff
alias off="shutdown now"

# octal permission
ols() { stat -c "%A %a %n" ${@} ; }

# timer
timer()
{
    if [ ${1} -ge 1 ] ; then
        for i in `seq ${1} -1 1` ; do
            echo -ne "\033[0K\r${i} seconds left" ;
            sleep 1 ;
        done
        echo -ne "\033[0K\rWaiting ${1} seconds\n" ;
    fi
}

# learn english 3,000 most common word
learn()
{
    # Choose word
    LINE=$(echo "$RANDOM % 3000 + 1" | bc)
    WORD=$(cat ~/.english.txt | sed -n $LINE"p")

    # Display in color the word
    echo -e "\033[1;31m"$WORD"\033[00m"

    # Translate
    trans -s en -t fr "$WORD"
}

# Debug mpi program
mpidebug() { mpirun -np $1 xfce4-terminal -e "gdb $2" ; }

# Copy line in file
copy()
{
    # Regular expression for integer
    re='^[0-9]*$'
    
    # Check number of argument
    if  [ $# == 2 ] ; then

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

# UI
alias tbmail="thunderbird 2> /dev/null &"
alias discord="discord 2> /dev/null &"
alias evince="evince 2> /dev/null "
alias scilab="~/Téléchargements/scilab-6.1.0/bin/scilab 2>/dev/null &"

#
## ex - archive extractor
## usage: ex <file>
ex ()
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
