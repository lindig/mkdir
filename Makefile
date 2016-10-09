#
#
#

ALL += 		mkdir.native
ALL += 		mkdir.html

.PHONY: all
all:		$(ALL)

clean:
		ocamlbuild -clean
		rm -f mkdir.ml mkdir.mli mkdir.md mkdir.html

mkdir.native: 	mkdir.ml mkdir.mli
		ocamlbuild -tag annot -pkg unix $@

mkdir.ml: 	mkdir.lp
		lipsum tangle -f cpp $@ $< > $@

mkdir.mli: 	mkdir.lp
		lipsum tangle -f cpp $@ $< > $@

%.md: 		%.lp
		lipsum weave $< | tr \\140 \\052 > $@

%.html: 	%.md
		theme $<

lipsum:
		opam install lipsum
