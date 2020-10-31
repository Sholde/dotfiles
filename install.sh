#!/bin/bash

# .emacs
cp -rf .emacs ~/.emacs

# scilab
mkdir -p ~/.emacs.d/own-mode
cp -rf highlight-scilab.el ~/.emacs.d/own-mode/highlight-scilab.el

# latex
cp -rf auto-complete-latex.el ~/.emacs.d/auto-complete-latex.el
