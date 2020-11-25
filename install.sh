#!/bin/bash

# .emacs
cp -rf .emacs ~/.emacs

# scilab
mkdir -p ~/.emacs.d/own-mode
cp -rf scilab-mode.el ~/.emacs.d/own-mode/scilab-mode.el

# latex
cp -rf auto-complete-latex.el ~/.emacs.d/auto-complete-latex.el

# message
echo "You need to install manually these packages :"
echo "- auto-complete"
echo "- yaml-mode"
echo "- cmake-mode"
echo "- badwolf-theme"
