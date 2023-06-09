################################################################################
.PHONY: all
all: clean install

################################################################################
.PHONY: install
install:
# Directories
	mkdir -p ~/.emacs.d ~/.emacs.d/own-mode

# .emacs
	cp -f .emacs ~/.emacs

# scilab
	cp -f scilab-mode.el ~/.emacs.d/own-mode/scilab-mode.el

# latex
	cp -f auto-complete-latex.el ~/.emacs.d/own-mode/auto-complete-latex.el

# mpi
	cp -f auto-complete-mpi.el ~/.emacs.d/own-mode/auto-complete-mpi.el

# pthread
	cp -f auto-complete-pthread.el ~/.emacs.d/own-mode/auto-complete-pthread.el


################################################################################
.PHONY: clean
clean:
	rm -rf *~
