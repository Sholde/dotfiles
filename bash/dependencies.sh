#!/bin/bash

# Test if terminal have 256 color
colors=$(tput colors)
if [ ${colors} -eq 256 ] ; then
    reset=$(tput sgr0)
    bold=$(tput bold)
    green=$(tput setaf 2)
    blue=$(tput setaf 4)
fi

# Do stuff here
list=$@
install=0
to_install=""

for l in ${list} ; do
    # Test if packages l is installed
    pkg-config --simulate ${l} > /dev/null
    res=$?

    # If not, add it to intall list
    if [ ${res} -ne 0 ] ; then
        to_install+="${l} "
        install=1
    fi
done

# Print packages to insall
if [ ${install} -ne 0 ] ; then
    # Print main message
    echo "${bold}${blue}""You need to install some packages:""${reset}"

    # Print packages
    for l in ${to_install} ; do
        echo "  ${bold}${green}->${reset} ${l}"
    done
fi

exit 0
