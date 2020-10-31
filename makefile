show=@

all: clean install

install: .emacs highlight-scilab.el auto-complete-latex.el
	$(show) ./install.sh

clean:
	$(show) rm -Rf *~
