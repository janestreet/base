# Generic Makefile for oasis project

VERSION=114.10+84

all:
	ocaml build/gen_config.ml > build/Makefile.config
	$(MAKE) -f build/Makefile.step1
	sed 's/VERSION/$(VERSION)/' META.in > META
	ocaml build/gen_install.ml > base.install

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
