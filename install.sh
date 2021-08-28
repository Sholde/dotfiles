#!/bin/bash

list_of_dir=$(ls -d */)

for dir in $list_of_dir ; do
    make -C $dir
done
