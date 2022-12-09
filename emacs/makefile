################################################################################
.PHONY: all
all: clean install

################################################################################
.PHONY: install
install:
# Directories
	mkdir -p ~/.emads.d ~/.emads.d/own-mode

# .emacs
	cp -f .emacs ~/.emacs

# aanila theme
	cp -f aanila-theme.el ~/.emacs.d/aanila-theme.el

# scilab
	cp -f scilab-mode.el ~/.emacs.d/own-mode/scilab-mode.el

# latex
	cp -f auto-complete-latex.el ~/.emacs.d/auto-complete-latex.el

# mpi
	cp -f auto-complete-mpi.el ~/.emacs.d/own-mode/auto-complete-mpi.el

# pthread
	cp -f auto-complete-pthread.el ~/.emacs.d/own-mode/auto-complete-pthread.el


################################################################################
.PHONY: clean
clean:
	rm -rf *~
