#######################################
#                Alias                #
#######################################

# Default
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

# Update with pacman
alias update="sudo pacman -Syu"

# emacs
alias emacs="emacs -nw"

# Find alias
alias fhere="find . -name $@"
alias fuser="find ~/ -name $@"
alias fsafe="find $@ 2> >(grep -v Permission)"

# locate command
alias locate="find / -name $1 2> >(grep -v Permission)"

# translate fr to en
alias transen="trans -s fr -t en $@"
    
# translate en to fr
alias transfr="trans -s en -t fr $@"

# search expression in all file here
alias search="grep -re $1 *"

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

# Debug mpi probram
alias mpidebug="mpirun -np \"$1\" xfce4-terminal -e \"cgdb ./$2\""

# Copy line in file
copy()
{
    # https://stackoverflow.com/questions/806906/how-do-i-test-if-a-variable-is-a-number-in-bash
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
