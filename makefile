all: clean install

install: .emacs highlight-scilab.el
	./install.sh

clean:
	rm -Rf *~
