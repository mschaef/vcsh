
.PHONY: all clean

all: vcsh-lang.html vcsh-lang.info

vcsh-lang.html: vcsh-lang.txi Makefile
	makeinfo --html --no-split vcsh-lang.txi -o vcsh-lang.html

vcsh-lang.info: vcsh-lang.txi Makefile
	makeinfo vcsh-lang.txi -o vcsh-lang.info

clean: 
		rm -f *~ vcsh-lang.info vcsh-lang.html