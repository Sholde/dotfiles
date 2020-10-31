all: clean install

install: .emacs highlight-scilab.el auto-complete-latex.el
	./install.sh

clean:
	rm -Rf *~
