#!/bin/bash

list_of_dir="bash emacs"

for dir in $list_of_dir ; do
    make -C $dir
done
