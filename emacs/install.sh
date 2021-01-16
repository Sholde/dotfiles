#!/bin/bash

# .emacs
cp -rf .emacs ~/.emacs

# aanila theme
cp -rf aanila-theme.el ~/.emacs.d/aanila-theme.el

# scilab
mkdir -p ~/.emacs.d/own-mode
cp -rf scilab-mode.el ~/.emacs.d/own-mode/scilab-mode.el

# latex
cp -rf auto-complete-latex.el ~/.emacs.d/auto-complete-latex.el

