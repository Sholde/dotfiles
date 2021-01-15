#!/bin/bash

# .emacs
cp -rf .emacs ~/.emacs
emacs --batch --eval "(progn 
                             (package-initialize) 
                             (package-refresh-contents)
                             (package-install 'badwolf-theme)
                             (package-install 'auto-complete)
                             (package-install 'cmake-mode)
                             (package-install 'org))"

# aanila theme
cp -rf aanila-theme.el ~/.emacs.d/aanila-theme.el

# scilab
mkdir -p ~/.emacs.d/own-mode
cp -rf scilab-mode.el ~/.emacs.d/own-mode/scilab-mode.el

# latex
cp -rf auto-complete-latex.el ~/.emacs.d/auto-complete-latex.el

