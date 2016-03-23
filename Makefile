default: ecaml.native

native: ecaml.native
byte: ecaml.byte

ecaml.byte ecaml.native:
	ocamlbuild -j 4 $@

# "make test" to see if anything broke
test: default
	cd tests && sh ./test.sh

# "make test-validate" to see if anything broke
# and ask for validation of possibly broken things.
test-validate: default
	cd tests && sh ./test.sh -v

clean:
	ocamlbuild -clean

.PHONY: clean ecaml.native test test-validate
