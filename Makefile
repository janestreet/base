# Generic Makefile for oasis project

NAME=base
VERSION=114.10+84
PREFIX=$(shell opam config var prefix)

all:
	ocaml build/gen_config.ml > build/Makefile.config
	$(MAKE) -f build/Makefile.step1
	sed 's/VERSION/$(VERSION)/' META.in > META
	ocaml build/gen_install.ml > base.install

install:
	opam-installer -i --prefix $(PREFIX) $(NAME).install

uninstall:
	opam-installer -u --prefix $(PREFIX) $(NAME).install

reinstall:
	opam-installer -u --prefix $(PREFIX) $(NAME).install &> /dev/null || true
	opam-installer -i --prefix $(PREFIX) $(NAME).install

bin.tar.gz: $(NAME).install
	rm -rf _install
	mkdir _install
	opam-installer -i --prefix _install $(NAME).install
	tar czf bin.tar.gz -C _install .
	rm -rf _install

bin.lzo: $(NAME).install
	rm -rf _install
	mkdir _install
	opam-installer -i --prefix _install $(NAME).install
	cd _install && lzop -1 -P -o ../bin.lzo `find . -type f`
	rm -rf _install
