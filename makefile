################################################################################
# Default target: make all
################################################################################
.PHONY: all
all:
	@echo "Try make help"

.PHONY: install
install:
	./install.sh

################################################################################
# Clean: removes everything that can be rebuilt
################################################################################
.PHONY: clean
clean:
	rm -Rf *~

################################################################################
# User help menu
################################################################################
.PHONY: help
help:
	@echo "################################################################### ";
	@echo "This is the Makefile for installing my dotfiles";
	@echo "make help shows this extra information";
	@echo "";
	@echo "Usage: make target";
	@echo "target is one of:";
	@echo "  help                      : show this help message";
	@echo "  all                       : nothing";
	@echo "  install                   : install the dotfiles";
	@echo "  clean                     : removes everything that can be rebuilt";
