#!/bin/bash

list_of_dir="bash emacs i3 git zathura"

for dir in $list_of_dir ; do
    make -C $dir
done
